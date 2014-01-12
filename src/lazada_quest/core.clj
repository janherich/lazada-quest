(ns lazada-quest.core
  (:require [ring.middleware.resource :refer :all]
            [ring.middleware.content-type :refer :all]
            [ring.middleware.not-modified :refer :all]
            [ring.adapter.jetty :as jetty]
            [ring.util.response :refer :all]
            [cheshire.core :refer :all]
            [lazada-quest.scrapper :as scrapper]
            [clojure.pprint :refer [pprint]])
  (:gen-class :main true))

;; define max products
(def MAX_PRODUCTS 5)

;; define filename for categories
(def FILE_NAME "main-site-categories-top.edn")

(defn save-to-file
  "Save category tree to file"
  [category-tree]
  (spit FILE_NAME (with-out-str (pprint category-tree))))

(defn generate-category-tree-file
  "If category tree file exists in filesystem, do nothing, otherwise
   scrape lazada website and save the generated category tree into file"
  [site-key priority-key]
  (when-not (.exists (clojure.java.io/as-file FILE_NAME))
    (-> (scrapper/create-category-tree site-key priority-key MAX_PRODUCTS)
        (save-to-file))))

(defn handler
  "Basic handler function, for every incoming request, the response is
   category tree from edn file converted to json format"
  [request]
  (let [categories (read-string (slurp FILE_NAME))]
    (-> (response (generate-string categories))
        (content-type "application/json"))))

(def webapp
  (-> handler
      (wrap-resource "public")
      (wrap-content-type)
      (wrap-not-modified)))

(defn start-server
  "Starts jetty-webserver on port 3000"
  [app]
  (jetty/run-jetty app {:port 3000 :join? false}))

(defn -main
  "Main entry point for the program"
  [& args]
  (do (generate-category-tree-file :main :top)
      (start-server webapp)))
