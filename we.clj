;;;;;;;;;;;;;;;;;
; Questions:
; - So wait, we can re-def something? Isn't everything immutable?
; - Am I using atom and @ correctly?
; - Use ref to support the mutual modification of *we-objects* and *we-branches*
; - Does it even make sense to have two dictionaries?
;
; To-do:
; - De/Re-construction of objects
; - Object diff
; - Web server/JSON layer
; - Comet for branches
; - DB visual explorer of some sort (is this just a we js view? hmm. probably.)
; - Better generate-id
; - CouchDB
; - History
; - Date/time
;
; Think:
; - Merge branches?
; - Ordering within a dictionary using "aabbba"-type keys
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

(defn load-object [object-id] 
  (load-revision (object-revision-id (to-int object-id))))

(defn ensure-object-id [object]
  (if (object :_object-id)
    object
    (let [object-id (generate-id)]
      (db-add! *we-objects* object-id nil)
      (assoc object :_object-id object-id))))

(defn save-object [object]
  (let [object1 (ensure-object-id object)]
    (let [object-id (object1 :_object-id)]
      (if (not= (object-revision-id object-id) (object1 :_revision-id))
	nil
	(let [new-id (generate-id)]
	  (db-add! *we-objects* object-id new-id)
	  (db-add! *we-revisions* new-id (assoc object1 :_revision-id new-id :_last-revision-id (object1 :_revision-id))))))))

(save-object {:name "Avital"})
(save-object (assoc (load-object 595751218) :last "Oliver2"))

@*we-objects*
@*we-revisions*

(defroutes greeter
  (GET "/object/:id" (json-str (load-object (params :id))))
  (POST "/object/:id" (save-object (read-json-string (params :doc))))
  (GET "/" (java.io.File. "/home/avital/we/core/core.html"))
  (GET "/view/*" (java.io.File. (str "/home/avital/we/views/" (params :*))))
  (GET "/*" (java.io.File. (str "/home/avital/we/core/" (params :*)))))

(run-server {:port 8080}
  "/*" (servlet greeter))



