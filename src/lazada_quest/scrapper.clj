(ns lazada-quest.scrapper
  (:require [clojure.string :as string]
            [clj-http.client :as client]
            [net.cgrand.enlive-html :as html]))

;; Define url parts for the site roots.
(def site-root-url-parts {:main {:url "http://www.lazada.com.ph"
                                 :name "Main Lazada Site"}})

;; Define url parts denoting priority ordering of the products, again define those url parts in a map
;; so we can select the priority ordering we are interested in for the current scraping run.
(def priority-url-parts {:new {:url "new-products"
                               :name "New products"}
                         :top {:url "top-sellers"
                               :name "Top sellers"}
                         :special {:url "special-price"
                                   :name "Special price"}})

;; Define url parts of the all top categories we are interested in, as we will always construct the whole
;; category tree, we will always use all top categories, therefore, is sufficient to store those url parts
;; in the set.
(def top-categories-url-parts #{"/shop-mobiles-tablets/"
                                "/shop-computers-laptops/"
                                "/shop-consumer-electronics/"
                                "/shop-cameras/"
                                "/shop-home-appliances/"
                                "/shop-health-beauty/"
                                "/shop-home-and-living/"
                                "/shop-toys-babies/"
                                "/shop-watches/"
                                "/shop-travel-luggage/"
                                "/sports/"
                                "/shop-books-music-movies/"})

(defn construct-url-string
  "Construct url string from the root, category and (optional) priority parts"
  ([root-part category-part]
     (str root-part category-part))
  ([root-part priority-part category-part]
     (str root-part "/" priority-part category-part)))

(defn fetch-url
  "Given some url string, fetch html content of the resource served under url adress and return
   it in the form of enlive nodes"
  [url]
  (html/html-resource (:body (client/get url {:as :stream}))))

(defn process-first-node-text
  "Given a sequence of enlive nodes, select text content of the first one and call process
   function on it"
  [nodes process-fn]
  (process-fn (html/text (first nodes))))

(defn read-number
  "Tries to read string containing US formatted number (with optional commas as thousands delimiters),
   if it's not possible to match given string for a number, exception is raised"
  [price-str]
  (if-let [[match num-str] (re-find #"^\D*?([0-9]{1,3}(?:,?[0-9]{3})*\.[0-9]{2})$" price-str)]
    (.parse (java.text.NumberFormat/getNumberInstance java.util.Locale/US) num-str)
    (throw (Exception. (str "bad amount format encountered, unable to parse price string: "
                            price-str)))))

(defn select-product-catalog-names-prices
  "Given the products site dom nodes and number n, fetch names and actual prices of the first n
   products on the site, return this information in the vector of maps where the product name is
   under :name key and actual product price is under :price key"
  [n products-site-dom]
  (let [product-nodes (take n (html/select products-site-dom [:section.catalog-products-section
                                                              :ul#productsCatalog
                                                              :li.unit]))]
    (into [] (pmap (fn [unit-dom]
                     {:name (process-first-node-text (html/select unit-dom [:em.itm-title])
                                                      (fn [name-str]
                                                        (-> name-str
                                                            (string/replace #"\n" "")
                                                            (string/trim))))
                      :price (process-first-node-text (html/select unit-dom [#{:div.itm-price
                                                                               :div.itm-price.special}])
                                                      read-number)})
                   product-nodes))))

(defn get-category-title
  "Find category title string in the given dom model"
  [site-dom]
  (html/text (first (html/select site-dom [:h1.catalog-title]))))

(defn process-level
  "Process one level of the url-parts in a tree"
  [tree url-parts continue-fn]
  (into tree (pmap (fn [url-part]
                     (let [products-site-dom (fetch-url url-part)]
                       {:name (get-category-title products-site-dom)
                        :children (continue-fn products-site-dom)})) url-parts)))

(defn create-tree-recursive
  "Recursively (mutual recursion together with function process-level) create a tree of
   all categories with the specified number of products in the each 'leaf' category"
  [level root-url-part priority-url-part max-products site-dom]
  (let [parent-list-element (keyword (apply str (interpose "." ["li"
                                                                (str "cnv-level-" level)
                                                                "childSelected"])))
        categories-list (first (html/select site-dom [parent-list-element :ul]))
        leaf-categories-urls (map (fn [{{product-url :href} :attrs}]
                                    (construct-url-string root-url-part priority-url-part product-url))
                                  (html/select categories-list [:li.cnv-level :a]))
        tree (process-level [] leaf-categories-urls (partial select-product-catalog-names-prices max-products))]
    (if-let [unfolded-categories-urls (seq (map (fn [{{product-url :href} :attrs}]
                                                  (construct-url-string root-url-part product-url))
                                                (html/select categories-list [:li.cnv-levelChilds :a])))]
      (process-level tree
                     unfolded-categories-urls
                     (partial create-tree-recursive (inc level) root-url-part
                              priority-url-part max-products))
      tree)))

(defn create-category-tree
  "Given site and priority ordering keywords, create category tree for 
   maximum of top n products in each category"
  [site-key priority-key max-products]
  (let [{site-root-url-part :url
         site-root-name :name}  (site-key site-root-url-parts)
        {priority-url-part :url
         priority-name :name} (priority-key priority-url-parts)]
    {:name (str site-root-name " - " priority-name)
     :children (into [] (pmap (fn [category-url-part]
                                (let [products-site-dom (fetch-url (construct-url-string site-root-url-part
                                                                                         priority-url-part
                                                                                         category-url-part))]
                                  {:name (get-category-title products-site-dom)
                                   :children (create-tree-recursive 1 site-root-url-part
                                                                    priority-url-part
                                                                    max-products
                                                                    products-site-dom)}))
                              top-categories-url-parts))}))
