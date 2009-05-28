;;;;;;;;;;;;;;;;;
; Questions:
; - Use ref to support the mutual modification of *we-objects* and *we-branches*
; - Does it even make sense to have two dictionaries?
;
; To-do:
; - Comet for branches
; - DB visual explorer of some sort (is this just a we js view? hmm. probably.)
; - Better generate-id
; - Date/time
;
; Think:
; - Merge branches?
;;;;;;;;;;;;;;;;;

(ns we
  (:use compojure)
  (:use clojure.contrib.json.write)
  (:use clojure.contrib.json.read)
  (:use clojure.contrib.duck-streams))

(defn to-int [str]
  (if (integer? str)
    str
    (try (Integer/parseInt str) 
	 (catch NumberFormatException nfe 0))))

(def *we-revisions* (atom {}))
(def *we-objects* (atom {}))

(defn generate-id [] 
  (int (* (rand) 1000000000)))

(defn db-add! [db id object]
  (swap! db assoc id object)
  object)

(defn object-revision-id [object-id] (@*we-objects* object-id))
(defn load-revision [revision-id] (@*we-revisions* revision-id))

(defn load-object-bare [object-id] 
  (load-revision (or (object-revision-id (to-int object-id)) object-id)))

(defn expand-value [value]
  (cond 
    (map? value) (load-object (value "_link"))
    (or (vector? value) (seq? value)) (map expand-value value)
    :else value))

(defn load-object [object-id]
  (let [object-bare (load-object-bare object-id)]
    (zipmap (keys object-bare) (map expand-value (vals object-bare)))))

(defn ensure-object-id [object]
  (if (object "_object-id")
    object
    (let [object-id (generate-id)]
      (db-add! *we-objects* object-id nil)
      (assoc object "_object-id" object-id))))

(defn ensure-valid-link [value]
  (cond 
    (and (map? value) (not (value "_link"))) {"_link" ((save-object value) "_object-id")}
    (vector? value) (doall (map ensure-valid-link value))
    :else value))
  
(defn save-subobjects [object]
  (zipmap (keys object)
	  (doall (map ensure-valid-link (vals object)))))

(defn save-object [object]
  (let [object1 (save-subobjects (ensure-object-id object))]
    (let [object-id (object1 "_object-id")]
      (if (not= (object-revision-id object-id) (object1 "_revision-id"))
	(throw (new java.util.ConcurrentModificationException))
	(let [new-id (generate-id)]
	  (db-add! *we-objects* object-id new-id)
	  (db-add! *we-revisions* new-id (assoc object1 "_revision-id" new-id "_last-revision-id" (object1 "_revision-id")))
	  (load-object object-id))))))

(defroutes greeter
  (GET "/object/:id" (json-str (load-object (params :id))))
  (GET "/object/:id/bare" (json-str (load-object-bare (params :id))))
  (POST "/object/:id" (try
		       (json-str (save-object (read-json-string (params :doc))))
	(catch Exception e "")))
  (GET "/new" (html [:html 
	       [:body 
		[:a 
		 {:href (str "/#" ((save-object {}) "_object-id"))}
		 "link (sorry for the bad hackiness)"]]]))
  (GET "/" (java.io.File. "/home/avital/we/core/core.html"))
  (GET "/view/*" (java.io.File. (str "/home/avital/we/views/" (params :*))))
  (GET "/*" (java.io.File. (str "/home/avital/we/core/" (params :*)))))

(run-server {:port 8080}
  "/*" (servlet greeter))

