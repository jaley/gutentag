(ns gutentag.core
  (:require [clojure.java.io :as io]
            [clojure.string  :as str]
            [gutentag.lucene :as luc]))

(def ^:const index-url "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/GUTINDEX.ALL")

(def ^:const books-base "http://www.mirrorservice.org/sites/ftp.ibiblio.org/pub/docs/books/gutenberg/")

(defn columns
  [^String line & offsets]
  (map (fn [[start end]]
         (when (<= 0 start end (.length line)) 
           (.. line (substring start end) trim)))
       offsets))

(defn process-line
  [line]
  (let [[desc id] (columns line [0 73] [73 78])]
    {:id          (try (Integer/parseInt id) (catch Exception _ nil))
     :description desc}))

(defn index-entries
  [lines]
  (let [index-start (drop-while (complement :id) (map process-line lines))]
    (reduce
      (fn [acc next-line]
        (if (:id next-line)
          (conj acc next-line)
          (update-in acc [(dec (count acc)) :description] str " ")))
      []
      index-start)))

(defn make-index
  [url]
  (with-open [rdr (io/reader (io/as-url index-url))]
    (reduce (fn [m {id :id desc :description}]
              (assoc m id desc))
            {}
            (index-entries (line-seq rdr)))))

(defn book-url
  [id]  
  (io/as-url
    (str
      books-base
      (str/join "/" (butlast (str id)))
      "/" id
      "/" id ".txt")))


