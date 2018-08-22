(ns crispy-docs.prod
  (:require [crispy-docs.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
