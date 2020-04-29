

Ideally you don't want to tie up a thread waiting for the result of each http request, so pmap or other thread-based approaches aren't really a good idea.

What you really want to do is:

    Fire off all the requests asynchronously
    Wait for the results with just one thread

My suggested approach is to use http-kit to fire off all the asynchronous requests at once, producing a sequence of promises. You then just need to dereference all these promises in a single thread, which will block the thread until all results are returned.

Something like:

```clojure
(require '[org.httpkit.client :as http])

(let [urls (repeat 100 "http://google.com") ;; insert your URLs here
      promises (doall (map http/get urls))
      results (doall (map deref promises))]
  #_do_stuff_with_results 
  (first results))
```
