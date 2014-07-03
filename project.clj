(defproject geo_annot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.0"]
                 [javax.servlet/servlet-api "2.5"] 
                 [compojure "1.1.8"]
                 [org.clojure/data.json "0.2.4"]
                 [hiccup "1.0.5"]
                 [http-kit "2.1.16"]]
  :jvm-opts  ["-Xmx1g" "-server"]
  :main geo-annot.core
  )
