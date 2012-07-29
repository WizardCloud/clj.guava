;; The usage of the eventbus is:
;; 1. create a eventbus
;;   (mk-eventbus)
;; 2. register your event handler function to the eventbus
;;   (register! event-name event-handler-fn) -- there is a default eventbus, so we dont need to add the eventbus here.
;; 3. then the event generator can post the event:
;;   (post! event-name event)  -- same as above, no need to pass eventbus, use default eventbus.


(ns ^{:doc "Clojure version of guava eventbus, it is totally reimplemented"
      :author "xumingmingv"}
  clj.guava.eventbus
  (:import [java.util.concurrent ConcurrentLinkedQueue]))

(defn- dispatch [eventbus]
  "Dispatches the event to handlers."
  (when-not @(:dispatching? eventbus)
    (reset! (:dispatching? eventbus) true)
    (try
      (let [^ConcurrentLinkedQueue events (:events eventbus)]
        (while (not (empty? events))
          (let [head-event (.poll events)
                handlers (:handlers eventbus)
                event-name (:event-name head-event)
                event-obj (:event head-event)
                this-handlers (@handlers event-name)]
            (when this-handlers
              (doseq [handler this-handlers]
                (handler event-obj))))))
      (finally
       (reset! (:dispatching? eventbus) false)))))

(defn mk-eventbus
  "Creates a new eventbus with the specified name, if name not provided :default will be used."
  {:added "0.1"}
  ([]
     (mk-eventbus :default))
  ([name]
     (let [eventbus {:name name
                     :handlers (atom {})
                     :events (ConcurrentLinkedQueue.)
                     :dispatching? (atom false)}]
       eventbus)))

(defn register!
  "Register the event-handler to handle the specified event"
  {:added "0.1"}
  [eventbus event-name handler]
  ;; TODO check the handler is a handler
  (let [handlers (:handlers eventbus)
        this-handlers (get-in @handlers [event-name])]
    (when-not this-handlers
      (swap! handlers assoc-in [event-name] []))
    (swap! handlers update-in [event-name] conj handler)))

(defn unregister!
  "Unregiser the event-handler."
  {:added "0.1"}
  [eventbus event-name handler]
  (let [handlers (:handlers eventbus)
        this-handlers (@handlers event-name)]
    (if this-handlers
      (swap! handlers update-in [event-name] #(vec (remove #{handler} %)))
      (throw (RuntimeException. (str "No such event registered: " event-name))))))

(defn post!
  "Post a event to the specified eventbus"
  {:added "0.1"}
  [eventbus event-name event]
  (let [^ConcurrentLinkedQueue events (:events eventbus)]
    (.add events {:event-name event-name :event event})
    (dispatch eventbus)))
