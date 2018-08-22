(ns ^:figwheel-no-load crispy-docs.dev
  (:require
    [crispy-docs.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
