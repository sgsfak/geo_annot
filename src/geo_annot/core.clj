(ns geo-annot.core
  (:gen-class)
  (:use [geo-annot.soft :as soft])
  (:use clojure.pprint)
  (:require [clojure.data.json :as json])
  (:require [hiccup.page :as page])
  (:use [compojure.route :only [files not-found]]
        [compojure.handler :only [site]] ; form, query params decode; cookie; session, etc
        [compojure.core :only [defroutes GET POST context]]
        org.httpkit.server))


(defn index-soft-records
  "Returns a set with the probe id as the key. The input should be the results of the parse-soft-file"
  [ parsed-soft]
  (let [{:keys [header records]} parsed-soft
        idx-fields (filter #(let [n (-> % clojure.string/upper-case)] 
                              (or (.endsWith n "ID")
                                  (= n "GB_ACC")
                                  (.contains n "SYMBOL") ))
                           header)
        make-idx (fn [field] (group-by #(-> % (get field "") clojure.string/upper-case) records))]
    {:header header
     :records records
     :idx (into {} 
                (for [f idx-fields]
                  [f (make-idx f)]))}))

(defn make-db-annot
  "It reads all SOFT files from the given directory and builds an 'annotation' index"
  [folder-name]
  (println "reading folder")
  (let [folder (java.io.File. folder-name)
        m (for [f (.listFiles folder) 
                :let [fname (clojure.string/upper-case (.getName f) )
                      platform (re-find #"GPL\d+" fname)]
                :when (and (.isFile f)
                           ;; (.endsWith fname ".SOFT")
                           platform)]

            (do
              (println "parsing " (.getName f))
              (let [k (->> f soft/parse-soft-file index-soft-records)]
                (hash-map platform  k)) ))]
    (into {} m)))

(defn available-platforms
  "Returns the platform identifiers that we have load their annotation from the SOFT files"
  [db-annot]
  (keys db-annot))

(defn platform-attributes
  [db-annot platform]
  (-> db-annot (get platform) :header))


(defn indexed-fields 
  [db-annot platform]
  (let [indices (-> db-annot (get platform) :idx)]
    (filter #(contains? indices %) (platform-attributes db-annot platform)) ))


(defn find-probe-in-platform
  "Given a 'annotation' index (db-annot), a platform identifier (e.g. GPL29431), and a probe id,
   it returns the matching information from the GEO"
  [db-annot platform probe-id]
  (let [cols (platform-attributes db-annot platform)
        hset (-> db-annot (get platform) :idx (get "ID"))
        d (-> probe-id clojure.string/upper-case hset)]
    (merge (into {} (for [c cols] [c nil]))
           d)))

(defn find-val-in-platform
  [db-annot platform k value]
  (let [p (-> db-annot (get platform) :records)
        matches? (fn [v]
                   (= (get v k) value))]
    (filter matches?  p)))

(defn find-val-in-platform-idx
  [db-annot platform k value]
  {:pre [(some #(= k %) (indexed-fields db-annot platform))]}
  (let [p (-> db-annot (get platform) :idx (get k))
        upcase-val (clojure.string/upper-case value)]
    (get p upcase-val [])))


(defn- json-response [dic]
  {:status 200
   :headers {"Content-Type" "text/plain"}
   :body (json/write-str dic)})
(defn get-probe-info
  [db req]
  (let [probe-id (-> req :params :probe)
        platforms (available-platforms db)
        platform (-> req :params :platform)
        cols (platform-attributes db platform)
        data (find-probe-in-platform db platform probe-id)]
    (json-response {:cols cols
                    :matches data })))

(defn get-avail-platforms
  [db req]
  (-> db available-platforms json-response))


(defn- template [db content]
  (page/html5
    [:head
     [:title "GEO Platform annotation search"]
     [:link {:rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"}]
     [:script "
      function chkbxsnegate() {
               var qsa = document.querySelectorAll('input[type=checkbox]')
               var l = qsa.length, i;
               for( i=0; i<l; i++) qsa[i].checked = !qsa[i].checked;
               }
      function chkbxsall() {
               var qsa = document.querySelectorAll('input[type=checkbox]')
               var l = qsa.length, i;
               for( i=0; i<l; i++) qsa[i].checked = 'checked'
               }
      "]
     ]
    [:body
     [:div.container
      [:div.row
       [:div.col-md-4
        [:h1 "Available Platforms"]
        [:ul]
        (for [p (available-platforms db)]
          [:li [:a {:href (str  "/" p)} p]]) ]
       [:div.col-md-8
        (for [t content] t) ]  ] ]]) 
  )
(defn home [db req]
  (template db
            [ [:h2 "Please select one of the platforms on the left..."] ]))

(defn platform-pg [db req]
  (let [platform (-> req :params :platform)
        idx-fields (indexed-fields db platform)
        fields (platform-attributes db platform) ]
    (template db
              [
               [:h1
                [:a {:href (str "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=" platform) :target "_blank"} platform]]
               [:form.horizontal-form {:method "post"}
                [:div.form-group 
                 [:label.col-sm-6.control-label {:for "genes"} "List of identifiers (one in each line)"]
                 [:textarea#genes.form-control {:name "genes" :rows "10"}] ]
                [:div.form-group
                 [:label.col-sm-2.control-label {:for "field"} "Search field"]
                 [:select {:name "field"}
                  (for [f idx-fields]
                    [:option {:value f} f]) ] ] 
                [:div.form-group
                 [:h3 "Select return fields"]
                 [:div.btn-group.btn-group-sm
                  [:a.btn.btn-info {:href "#", :onclick "chkbxsall();return false"
                                    :role "button" }
                   [:span.glyphicon.glyphicon-check]
                   "ALL" ]
                  [:a.btn.btn-info {:href "#", :onclick "chkbxsnegate();return false"
                                    :role "button" }
                   [:span.glyphicon.glyphicon-random]
                   "Toggle"]]
                 (for [f fields]
                   [:div.checkbox [:input {:type "checkbox" :name "ret[]" :value f
                                           :checked "checked"}]
                    f ])   ]

                [:button.btn.btn-success "Submit"]
                ]] )  )) 

(defn search-platform [db req]
  (let [f (-> req :params :field)
        lines (-> req :params :genes clojure.string/split-lines)
        genes (map clojure.string/trim lines)
        plat (-> req :params :platform)
        fields-to-return (-> req :params :ret)
        cols fields-to-return ;;(platform-attributes db plat)
        ;; results (for [g genes] (find-val-in-platform db plat f g))
        results (mapcat #(find-val-in-platform-idx db plat f %) genes) ]
    (pprint genes)
    (template db
              [[:h1 "Results"]
               [:table.table.table-striped
                [:thead
                 [:tr (for [k cols]
                        [:th k])]]
                [:tbody
                 ;;(for [res results] (for [r res] [:tr (map (fn [c] [:td  (get r c)]) cols ) ]))
                 (for [r results] 
                   [:tr (map (fn [c] [:td  (get r c)]) cols ) ])
                 
                 ]]])

    )
  )
(defn app-routes [db]
  (compojure.core/routes
    (GET "/" [] #(home db %))
    (GET "/platforms" [] #(get-avail-platforms db %))
    (GET "/:platform" [] #(platform-pg db %))
    (POST "/:platform" [] #(search-platform db %)) 
    (files "/static/") ;; static file url prefix /static, in `public` folder
    (not-found "<p>Page not found.</p>")))

(defn -main [& args]
(let [app (-> (make-db-annot "platforms")  
              app-routes
              site) ]
  (run-server app {:port 9797})))

