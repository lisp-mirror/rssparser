;;; Web to RSS Parser
;;; by tux. [ http://tuxproject.de ]
;;;
;;; Licensed under the terms of the WTFPL.
;;; http://wtfpl.net/txt/copying/


(in-package #:rssparser)


;; Make Parenscript coexist with CL-WHO
(setf parenscript:*js-string-delimiter* #\")

;; Set the output doctype
(setf (html-mode) :html5)


(defun print-main-html ()
  "Prints the index file for the web server, containing JS and CSS."
  (with-html-output-to-string
      (s nil :indent t)

    (:html
     (:head
      (:title "RSSParser Web Control Center")
      (str (generate-prologue *ajax-processor*))
      (:style :type "text/css"
              (str (compile-and-write
                    '(body
                      :margin 0
                      :padding 0
                      :font-family monospace
                      :font-size 12px)
                    '(h1
                      :margin 5px)
                    '(table
                      :margin 10px
                      :border-collapse collapse
                      (tr
                       (td
                        :border 1px solid darkgray
                        :padding 4px)
                       (td.centered
                        :text-align center)))
                    '(div#footer
                      :position fixed
                      :width 100%
                      :padding 5px
                      :display block
                      :bottom 0px
                      :left 0px))))
      (:script :type "text/javascript"
               (str (ps
                     (defun update-table (htmltext)
                       (setf (chain document (get-element-by-id "ajaxtable") inner-h-t-m-l) htmltext))

                     (defun request-table ()
                       (chain smackjack (htmltable update-table))
                       nil)

                     (defun zap-feed (feedid)
                       (chain smackjack (delfeed feedid))
                       (request-table)
                       nil)))))
     (:body
      (:h1 "RSSParser Web Control Center")
      (:div :id "ajaxtable")
      (:div :id "footer"
            (:a :href "http://bitbucket.org/tux_/rssparser.lisp" :target "_blank" "Powered by RSSParser.lisp"))
      (:script :type "text/javascript"
               (str (ps (request-table))))))))
