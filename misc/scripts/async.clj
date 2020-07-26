(require '[org.httpkit.client :as http])

(let [urls (repeat 100 "http://google.com") ;; insert your URLs here
      promises (doall (map http/get urls))
      results (doall (map deref promises))]
  #_do_stuff_with_results 
  (first results))
