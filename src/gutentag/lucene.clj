(ns gutentag.lucene
  (:import [org.apache.lucene.analysis.standard StandardAnalyzer]
           [org.apache.lucene.document Document Field Field$Store IntField TextField]
           [org.apache.lucene.index DirectoryReader IndexWriter IndexWriterConfig]
           [org.apache.lucene.queryparser.classic QueryParser]
           [org.apache.lucene.search IndexSearcher Query ScoreDoc TopScoreDocCollector]
           [org.apache.lucene.store RAMDirectory]
           [org.apache.lucene.util Version]))

(def analyzer (StandardAnalyzer. Version/LUCENE_40))

(def directory (RAMDirectory.))

(defn add-book!
  [writer id description]
  (let [doc (Document.)]
    (.add doc (IntField. "id" (int id) Field$Store/YES))
    (.add doc (TextField. "description" description Field$Store/YES))
    (.addDocument writer doc)))

(defn add-books!
  [books]  
  (let [cfg (IndexWriterConfig. Version/LUCENE_40, analyzer)]
    (with-open [writer (IndexWriter. directory cfg)]
      (doseq [[id desc] books]        
        (when (and id desc) 
          (add-book! writer id desc))))))

(defn search
  [qry-str]
  (let [query (.parse (QueryParser. Version/LUCENE_40 "description" analyzer)
                qry-str)]
    (with-open [rdr (DirectoryReader/open directory)]
      (let [searcher  (IndexSearcher. rdr)
            collector (TopScoreDocCollector/create 10 true)]
        (.search searcher query collector)
        (doall
          (for [hit (. (.topDocs collector) scoreDocs)
                :let [doc (.doc searcher (. hit doc))]]
            {:id          (.get doc "id")
             :description (.get doc "description")}))))))
