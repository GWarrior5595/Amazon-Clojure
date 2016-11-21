(ns isbn-finder.core
  (:gen-class)
  (:use clojure.java.io)
  (:require [pradpi.core :refer :all])
  (:import java.util.concurrent.Executors))

(defn ljust [string width]
  (str string (apply str (repeat (- width (count string)) \space))))

(defn result [isbn]
            (def config {:associate-tag "seangrahamaws-20"
                          :key-id "AKIAIMKJHSGQA6ASXMPQ"
                          :secret "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"})
                          
            (item-lookup config {:IdType "ISBN"
                                 :SearchIndex "Books"
                                 :ResponseGroup "Large"
                                 :ItemId isbn}
                                 ))
(defn String->Number [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))

(defn read-file [fname]
  (with-open [rdr (reader fname)]
    (doall (line-seq rdr))))

(defn call-amazon-api-by-isbn [isbn]
  (def isbn-map @(result isbn))
  (def title (get-in isbn-map [:items :item 0 :item-attributes :title]))
  (def salesrank (get-in isbn-map [:items :item 0 :sales-rank]))
  
  (if salesrank
    (def new-salesrank (String->Number salesrank))
    (def new-salesrank nil))

  (def book-details {:title title
                      :isbn isbn
                      :salesrank new-salesrank}) 
  
  book-details)

(defn order-list-by-salesrank [list]
  (sort-by :salesrank list))

(defn print-results [list]
  (def book-title-padding 70)
  (def isbn-padding 15)

  (println (ljust "book title" book-title-padding)
            (ljust "isbn" isbn-padding)
            "rank")

  (doseq [book list]
    (println (ljust (get book :title) book-title-padding) 
              (ljust (get book :isbn) isbn-padding) 
                      (get book :salesrank))))

(defn sequential-isbn-finder [isbn-list]
  (mapv call-amazon-api-by-isbn isbn-list))

(defn add-book [books-ref isbn]
  (let [book (call-amazon-api-by-isbn isbn)]
  (dosync (alter books-ref conj book))))

(defn concurrent-isbn-finder [isbn-list]
  (def  books(ref []))
  (let [
        pool (Executors/newFixedThreadPool (count isbn-list)) 
        tasks (map (fn [isbn] (fn [] (add-book books isbn)))
        isbn-list)]
  (.invokeAll pool tasks)
  (.shutdown pool)
  (.awaitTermination pool 10 java.util.concurrent.TimeUnit/MINUTES))
  @books)

(defn isbn-finder [isbn-list isbn-finder]
  (def list-objects (order-list-by-salesrank (isbn-finder isbn-list)))

  (print-results list-objects))

(defn -main [& args]
  (def isbn-list (read-file (first args)))

  (println "Sequential:")
  (time (isbn-finder isbn-list sequential-isbn-finder))

  (println "Sleeping for five seconds to avoid amazon rejecting concurrent api calls.")
  (Thread/sleep 5000)

  (println "Concurrent:")
  (time (isbn-finder isbn-list concurrent-isbn-finder))

  (shutdown-agents))



