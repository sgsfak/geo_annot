(ns geo-annot.soft)

(defn- split-line-in-tabs
  [line]
  (clojure.string/split line #"\t"))

(defn- parse-data-table [lines]
  (for [line lines :while (not= line "!platform_table_end")]
    (split-line-in-tabs line)) )

(defn- parse-lines-ignore-hdr [lines]
  (let [first-line (first lines)
        rest-lines (next lines)]
    (if (= "!platform_table_begin" first-line)
      #(parse-data-table rest-lines)
      #(parse-lines-ignore-hdr rest-lines))))

(defn- parse-soft-format
  [f]
  (let [lines (line-seq (clojure.java.io/reader f))]
    (trampoline parse-lines-ignore-hdr lines)))

(defn parse-soft-file
  "It parses the given file in SOFT format and returns a  seq of its data table"
  [file]
  (let [v (parse-soft-format file)
        cols (first v)
        data (next v)
        records (for [rec data]
                  (apply merge
                         (map hash-map cols rec) ))]
    {:header cols
     :records records}))
