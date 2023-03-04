(ns vish.vish
  (:require [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [clj-time.core :as t]
            [clj-time.format :as tf]))

(defn ts-to-s
  [ts]
  (reduce + (map *
                 (map #(Float/parseFloat %) (str/split ts #":"))
                 [3600 60 1])))

(defn duration-ffprobe
  [video-path]
  (Float/parseFloat
   (str/trim
    (:out (sh "ffprobe" "-v" "error" "-select_streams" "v:0" "-show_entries" "format=duration" "-of" "default=noprint_wrappers=1:nokey=1" "-i" video-path)))))

(defn duration
  [video-path]
  (ts-to-s
   (nth (re-find #"Duration\:\s(\d?\d\d\:\d\d\:\d\d\.\d+)\,"
                 (:err (sh "ffmpeg" "-v" "32" "-hide_banner" "-i" video-path))) 1)))


(duration-ffprobe "./video.mkv")
(duration "./video.mkv")



(comment

  :rcf)
