(ns vish.vish
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str])
  (:import javax.imageio.ImageIO
           java.io.ByteArrayInputStream
           dev.brachtendorf.jimagehash.hashAlgorithms.PerceptiveHash))


(defn ts-to-s
  [ts]
  (reduce + (map *
                 (map #(Float/parseFloat %)
                      (str/split ts #":"))
                 [3600 60 1])))

(defn duration-ffprobe
  [video-path]
  (Float/parseFloat
   (str/trim
    (:out (sh
           "ffprobe" "-v" "error"
           "-select_streams" "v:0"
           "-show_entries" "format=duration"
           "-of" "default=noprint_wrappers=1:nokey=1"
           "-i" video-path)))))

(defn duration
  [video-path]
  (ts-to-s
   (nth (re-find #"Duration\:\s(\d?\d\d\:\d\d\:\d\d\.\d+)\,"
                 (:err (sh
                        "ffmpeg" "-v" "32" "-hide_banner"
                        "-i" video-path))) 1)))


(defn extract-frame
  [video-path timestamp frame-size]
  (:out (sh
         "ffmpeg" "-v" "1"
         "-ss" (str timestamp)
         "-i" video-path
         "-frames:v" "1"
         "-s" (format "%sx%s" frame-size frame-size)
         "-f" "image2pipe" "-"
         :out-enc :bytes)))

(defn img-from-out
  [out]
  (ImageIO/read (ByteArrayInputStream. out)))

(defn get-frame
  [video-path timestamp
   & {:keys [frame-size]
      :or {frame-size 240}}]
  (img-from-out (extract-frame video-path timestamp frame-size)))


(defn timestamps
  [duration n]
  (let [step-size (/ duration (+ n 1))]
    (map #(+ step-size (* % step-size)) (range n))))


(defn ints-to-ulonglong
  [ints]
  (let [parts (count ints)
        size (/ 64 parts)] ;; infer input int bit-size by number of ints
    (reduce bit-or
            (map #(bit-shift-left (nth ints %) (* size (- (dec parts) %)))
                 (range parts)))))

(defn conc-ints
  [ints intsize]
  (let [chunks (partition (/ 64 intsize) ints)]
    (map ints-to-ulonglong chunks)))

(defn combine-integers
  [integers]
  (let [chunks (partition 4 integers)
        longs (reduce (fn [acc chunk]
                        (let [a (bit-shift-left (nth chunk 0) 48)
                              b (bit-shift-left (nth chunk 1) 32)
                              c (bit-shift-left (nth chunk 2) 16)
                              d (nth chunk 3)]
                          (conj acc (bit-or (bit-or a b) (bit-or c d)))))
                      [] chunks)]
    longs))


(defn phash
  [img]
  (int (.getHashValue (.hash (PerceptiveHash. 16) img))))


(defn videohash
  [video-path]
  (conc-ints (pmap phash
                   (pmap #(get-frame video-path %)
                         (timestamps (duration video-path) 16))) 16))


(comment
  (duration-ffprobe "./video.mkv")
  (duration "./video.mkv")

  (ImageIO/read (java.io.File. "collage.jpg"))

  (:out (sh "ffmpeg" "-v" "1" "-ss" (str 2) "-i" "./video.mkv" "-frames:v" "1" "-s" (format "%sx%s" 240 240) "-f" "image2pipe" "-" :out-enc :bytes))

  (type (:out (sh "ffmpeg" "-v" "1" "-ss" (str 2) "-i" "./video.mkv" "-frames:v" "1" "-s" (format "%sx%s" 240 240) "-f" "image2pipe" "-" :out-enc :bytes)))

  (ByteArrayInputStream. (:out (sh "ffmpeg" "-v" "1" "-ss" (str 2) "-i" "./video.mkv" "-frames:v" "1" "-s" (format "%sx%s" 240 240) "-f" "image2pipe" "-" :out-enc :bytes)))

  (ImageIO/read (ByteArrayInputStream. (:out (sh "ffmpeg" "-v" "1" "-ss" (str 2) "-i" "./video.mkv" "-frames:v" "1" "-s" (format "%sx%s" 240 240) "-f" "image2pipe" "-" :out-enc :bytes))))

  (img-from-out (:out (sh "ffmpeg" "-v" "1" "-ss" (str 2) "-i" "./video.mkv" "-frames:v" "1" "-s" (format "%sx%s" 240 240) "-f" "image2pipe" "-" :out-enc :bytes)))

  (get-frame "./video.mkv" 2)
  (get-frame "./video.mkv" 2.5 :frame-size 120)

  (timestamps 52.0800 16)

  (def hash-length 256)
  (def sample-count 16)
  (def sample-hash-size (/ hash-length sample-count))
  (.hash (PerceptiveHash. sample-hash-size) (get-frame "./video.mkv" 3))
  (.getHashValue (.hash (PerceptiveHash. sample-hash-size) (get-frame "./video.mkv" 2)))
  (.toByteArray (.hash (PerceptiveHash. sample-hash-size) (get-frame "./video.mkv" 2)))
  (phash (get-frame "./video.mkv" 2))

  (defn combine-integers [hi lo hi-bits lo-bits]
    (let [shifted-hi (bit-shift-left (int hi) lo-bits)
          mask (- (bit-shift-left 1 (+ hi-bits lo-bits)) 1)
          result (bit-and (bit-or shifted-hi (int lo)) mask)]
      result))

  (bit-shift-left 1 16)

  (/ 64 16)

  (combine-integers [76 59 64 16])
  (range 16)


  (defn timestamps
    [duration n]
    (let [step-size (/ duration (+ n 1))]
      (map #(+ step-size (* % step-size)) (range n))))

  (defn combine-integers
    [integers]
    (let [chunks (partition 4 integers)
          longs (reduce (fn [acc chunk]
                          (let [a (bit-shift-left (nth chunk 0) 48)
                                b (bit-shift-left (nth chunk 1) 32)
                                c (bit-shift-left (nth chunk 2) 16)
                                d (nth chunk 3)]
                            (conj acc (bit-or (bit-or a b) (bit-or c d)))))
                        [] chunks)]
      longs))

  (combine-integers [0 0 0 0 0 0 0 0 0 0 0 0 0 0 76 59])
  (combine-integers [27706 39008 65103 4253])

  (bit-shift-left 76 8)

  (def ints [0 0 0 0 0 0 0 0 0 0 0 0 0 0 76 59])
  ;; (def parts (/ hash-length 64))
  (def chunks (partition parts ints))
  chunks
  (range parts)
  (def c (nth chunks 3))
  (def ulonglongparts (map #(bit-shift-left (nth c %) (* sample-hash-size (- (dec parts) %))) (range parts)))
  ulonglongparts
  (def ulonglong (reduce bit-or ulonglongparts))

  (defn ints-to-ulonglong
    [ints]
    (let [parts (count ints) ;; infer input int bit-size by number ints
          size (/ 64 parts)]
      (reduce bit-or (map #(bit-shift-left (nth ints %) (* size (- (dec parts) %))) (range parts)))))
  (ints-to-ulonglong [0 0 0 0 0 0 76 59])


  :rcf)

(comment
  (def video-path "./video.mkv")
  (def h1 (phash (get-frame "./video.mkv" 1)))
  (type h1)
  h1
  (int h1)
  (print h1)
  (def h2 (phash (get-frame "./video.mkv" 3)))
  (print h2)
  (def concd (conc-ints [0 0 h1 h2] 16))
  (print concd)

  (map phash
       (pmap #(get-frame video-path %)
             (timestamps (duration video-path) 16)))

  (conc-ints (pmap phash
                   (pmap #(get-frame video-path %)
                         (timestamps (duration video-path) 16))) 16)

  (videohash "./video.mkv")

  (def files ["./video.mkv" "./video.mp4"])
  (pmap videohash files)
  
  :rcf)
