#!/usr/local/bin/sbcl --script

;;; Web to RSS Parser
;;; by tux. [ http://tuxproject.de ]
;;;
;;; Licensed under the terms of the WTFPL.
;;; http://wtfpl.net/txt/copying/


;;; valid syntax:
;;;  * rssparser add <Title> <URL> <EntrySelector> <TitleSelector> [<ContentSelector>]
;;;  * rssparser del(ete) <ID>
;;;  * rssparser list
;;;  * rssparser parse


;;; PACKAGE SETUP

(load "~/quicklisp/setup.lisp")

(ql:quickload '(:datafly        ;; for database access
                :cl-ppcre       ;; for regex
                :dexador        ;; for using the web
                :clss           ;; for CSS selecting
                :plump          ;; for parsing XML/DOM
                :plump-sexp     ;; for converting DOM to S-exps
                :local-time     ;; for time conversion
                :xml-emitter)   ;; for creating the RSS files
             :silent t)

(defpackage #:rssparser
  (:use :cl :sxql :datafly :xml-emitter :local-time
    #+sbcl :sb-int
    #+ccl :ccl))

(in-package #:rssparser)



;;; CONSTANTS AND PARAMETERS


(defun get-args ()
  "Returns the list of command-line parameters"
  (or
   #+sbcl sb-ext:*posix-argv*
   #+ccl *command-line-argument-list*
   nil))


;;; Store the script's command mode in a parameter
;;; so we won't have to fetch it again and again.
;;; Also store the arguments.
(defparameter *script-mode* (cadr (get-args)))
(defparameter *script-arguments* (cddr (get-args)))


;;; Store the database file name and the folder
;;; for our feed files so we can easily change
;;; them later if we want to ...
(defconstant +database-file+ "feeds.db")
(defconstant +feed-folder+ "feeds/")


;;; Store the maximum number of entries per feed.
(defconstant +max-items-per-feed+ 50)


;;; Set up a feed cleaner: If this constant is not NIL,
;;; the feed parser will remove old entries from the
;;; database automatically.
(defconstant +feed-cleanup+ t)


;;; If a website is dead, it could automatically be
;;; removed from the feed list.
(defconstant +remove-dead-feeds+ t)



;;; HELPER FUNCTIONS


(defun show-syntax ()
  "Prints the command-line syntax for the RSS parser"
  (format t "Syntax:~% * rssparser.lisp add <Title> <URL> <EntrySelector> <TitleSelector> [<ContentSelector>]~% * rssparser.lisp delete <ID>~% * rssparser.lisp list~% * rssparser.lisp webserver~%~%If you're a bot:~% * rssparser.lisp parse"))


(defun print-feed-list (list)
  "Makes the feed list look nicer."
  (loop for feed in list do
    (let ((id-pair (first feed))
          (title-pair (second feed))
          (url-pair (third feed))
          (lastsuccess-pair (fourth feed)))

     (format t "~%ID: ~a  Title:        ~a~%        URL:          ~a~%        Last success: ~a~%"
       (cdr id-pair)
       (cdr title-pair)
       (cdr url-pair)
       (if (cdr lastsuccess-pair)
         (timestamp-to-rssdate (cdr lastsuccess-pair))
         ;; New feeds don't have a "last success" yet:
         "-")))))


(clss:define-pseudo-selector external-link (node)
  "CLSS pseudo-selector for external links, shamelessly borrowed from the CLSS manual."
  (let ((href (plump:attribute node "href")))
    (and href (cl-ppcre:scan "^(http|https)://" href))))


(defun extract-urls (node)
  "Tries to find an external href in <node>. Returns a list of all found URLs."
  (let ((link-list (clss:select "a:external-link" node)))
    (loop for found-link being the elements of link-list
          collect (list (plump:attribute found-link "href")))))


(defun timestamp-to-rssdate (timestamp)
  "Returns a readable date from a given timestamp."
  (format-timestring nil
    (unix-to-timestamp timestamp)
    :format +rfc-1123-format+))



;;; MAIN APPLICATION


;; Force the charset to be Unicode
#+ccl (setf *default-external-format* :UTF-8)
#+sbcl (setf sb-impl::*default-external-format* :UTF-8)

(connect-toplevel :sqlite3 :database-name +database-file+)

(defun add-new-feed (params)
  "Adds a new feed to the database."
  ;; Params: Feed title, URL, entry selector, title sel., [ content sel. ]
  (if
    (and
      ;; URL given?
      (cl-ppcre:scan "^https?://" (second params))
      ;; Are the necessary params given at all?
      (>= (list-length params) 4)
      (<= (list-length params) 5))
    (progn
      (if
        (and
          ;; Everything but the title must not be a number.
          (not (numberp (parse-integer (second params) :junk-allowed t)))
          (not (numberp (parse-integer (third params) :junk-allowed t)))
          (not (numberp (parse-integer (fourth params) :junk-allowed t))))
        (progn
          (let ((content 
            (if (fifth params)
                (princ-to-string (fifth params))
                "Generated with rssparser.lisp.")))
            (execute
              ;; all arguments are set (probably even correctly).
              (insert-into :feeds
                (set= :feedtitle (princ-to-string (first params))
                      :url (princ-to-string (second params))
                      :entryselector (princ-to-string (third params))
                      :titleselector (princ-to-string (fourth params))
                      :contentselector content))))
          t)
        nil))
    (progn
      ;; invalid number of arguments
      (show-syntax)
      nil)))


(defun delete-feed (id)
  "Deletes a feed and all of its references."
  (if
   (and
    (eql 1 (list-length id))
    (numberp (parse-integer (car id) :junk-allowed t)))
   (let ((id-to-delete (car id)))
     ;; The <id-to-delete> is the number of the feed to delete.
     (if
      (retrieve-one (select :id (from :feeds) (where (:= :id id-to-delete))))
      (progn
        ;; This feed exists. Remove it.
        (execute (delete-from :feeds
                  (where (:= :id id-to-delete))))
        (execute (delete-from :entries
                  (where (:= :feedid id-to-delete))))
        ;; Now that the database entry is gone, we don't
        ;; need the XML file anymore.
        (let
          ;; The file is probably "feeds/feed<ID>.xml".
          ((feed-file (probe-file (concatenate 'string +feed-folder+ "feed" id-to-delete ".xml"))))
            (if feed-file
              ;; We have an XML file. Yet.
              (delete-file feed-file)))
        t)
      ;; No such feed in the database
      nil))
   (progn
     ;; invalid number of arguments or ID is NaN
     (show-syntax)
     nil)))


(defun list-all-feeds ()
  "Lists all known feeds."
  (retrieve-all
    (select (:id :feedtitle :url :lastsuccess)
      (from :feeds))
   :as 'trivial-types:association-list))


(defun parse-all-feeds ()
  "Loops over the known feeds and generates the XML files."
  ;(declare (optimize (compilation-speed 3) (speed 3) (safety 0)))
  (loop for feed in
       (retrieve-all
        (select (:feedtitle :url :entryselector :titleselector :contentselector :id)
                (from :feeds)))
     do
       (let
           ((feed-title (second feed))
            (feed-url (fourth feed))
            (feed-entries (sixth feed))
            (feed-entry-titles (eighth feed))
            (feed-entry-contents (tenth feed))
            (feed-id (last feed)))
         (handler-case
             (let ((the-dom (plump:parse (dex:get feed-url))))
               ;; the-dom is the DOM of our feed URL now.
               ;; We can process it normally.
               (let ((box-elements (clss:select feed-entries the-dom))
                     (feed-file (concatenate 'string +feed-folder+ "feed" (princ-to-string (car feed-id)) ".xml")))
                 (with-open-file (stream feed-file
                                         :direction :output
                                         :if-exists :overwrite
                                         :if-does-not-exist :create)
                   (with-rss2 (stream :encoding "utf-8")
                     (rss-channel-header feed-title feed-url)
                     (loop for feed-entry being the elements of box-elements do
                          (let
                              ((contents
                                ;; If the feed has an "empty" contents selector, take the
                                ;; whole box element's content as the entry contents
                                (if (string= feed-entry-contents "")
                                    (list feed-entry)
                                    (clss:select feed-entry-contents feed-entry)))

                               ;; The title elements are always stored with the feed.
                               (titles (clss:select feed-entry-titles feed-entry)))

                            ;; Loop over all titles/contents and add an RSS item for each of them
                            (loop for single-title being the elements of titles
                                  for single-contents being the elements of contents
                               do
                                 (let
                                     ;; Grab the plain text from the title:
                                     ((our-title (plump:text single-title))
                                      ;; Try to find the entry URL:
                                      (our-url (extract-urls single-title))
                                      ;; Serialize the contents to NIL (return a string):
                                      (our-contents (plump:serialize (plump-sexp:parse (plump-sexp:serialize single-contents)) nil)))
                                   ;; Write the data to the database:
                                   (when
                                     (and
                                        our-title
                                        our-contents

                                        ;; Only do it if we don't have this item yet :-)
                                        (not (retrieve-all
                                          (select :*
                                            (from :entries)
                                            (where (:and
                                              (:= :feedid (car feed-id))
                                              (:= :title our-title)))))))

                                       (execute
                                         (insert-into :entries
                                           (set= :feedid (princ-to-string (car feed-id))
                                                 :title (princ-to-string our-title)
                                                 :contents (princ-to-string our-contents)
                                                 :url (if our-url (princ-to-string (caar our-url)) feed-url)
                                                 :timestamp (timestamp-to-unix (now)))))))

                                   ;; Update the success timestamp in the database.
                                   (execute
                                     (update :feeds
                                       (set= :lastsuccess (timestamp-to-unix (now)))
                                       (where (:= :id (car feed-id))))))))

                    ;; Clean up if requested.
                    (when +feed-cleanup+
                      (execute
                        (delete-from :entries
                          (where (:in :id
                            (select :id
                              (from :entries)
                              (where (:= :feedid (car feed-id)))
                              (order-by (:desc :timestamp))
                              (limit -1)
                              (offset (* 2 +max-items-per-feed+))))))))

                    ;; We have a nicely filled database now.
                    ;; Grab the newest entries and put them into our feed.
                    (loop for item-list in
                      (retrieve-all
                        (select (:title :contents :url :timestamp)
                          (from :entries)
                          (where (:= :feedid (car feed-id)))
                          (order-by (:asc :timestamp))
                          (limit +max-items-per-feed+))
                      :as 'trivial-types:association-list)
                    do
                      ;; Our list should look as follows now:
                      ;; ((TITLE) (CONTENTS) (URL) (TIMESTAMP))
                      (let ((this-title (cdr (first item-list)))
                            (this-contents (cdr (second item-list)))
                            (this-url (cdr (third item-list)))
                            (this-timestamp (cdr (fourth item-list))))

                        ;; Write each fetched item to the RSS file.
                        (rss-item (princ-to-string this-title)
                                  :link this-url
                                  :description (princ-to-string this-contents)
                                  :pubDate (princ-to-string (timestamp-to-rssdate this-timestamp)))))))))

           (simple-file-error ()
             ;; The feed folder is not writeable.
             (format t "Please fix the access rights for ~a for this script to work.~%" +feed-folder+)
             (return nil))

           (dex:http-request-service-unavailable ()
             ;; Temporary website error. Retry later.
             (format t (concatenate 'string "~%Feed " (prin1-to-string (car feed-id)) " has a website which is "
                                            " temporarily unavailable. We'll try later.")))
  
           (dex:http-request-failed (e)
             ;; Page not found. Assume it is gone.
             (if +remove-dead-feeds+
               (progn
                 ;; Remove the page from the feeds.
                 (format t (concatenate 'string
                                        "~%Feed " (prin1-to-string (car feed-id)) " seems to have a broken website: "
                                        feed-url " could not be reached (" e "). We'll better remove it."))
                 (force-output nil)
                 (delete-feed (list (write-to-string (car feed-id)))))
               (progn
                 ;; Display a warning.
                 (format t (concatenate 'string
                                        "~%Feed " (prin1-to-string (car feed-id)) " seems to have a broken website: "
                                        feed-url " could not be reached (" e ")."))
                 (force-output nil))))))))


(defun start-webserver ()
  "Runs the RSS parser's built-in web server."
  ;; Comes soon (tm)
)


(defun rssparser ()
  "The main function, evaluating the command-line parameters."
  (cond
    ((string= *script-mode* "add")
        ;; add an entry
        (when
          (add-new-feed *script-arguments*)
          (format t "Success!")))
    ((or
      (string= *script-mode* "delete")
      (string= *script-mode* "del"))
        ;; remove an entry
        (if
          (delete-feed *script-arguments*) 
          (format t "Deletion successful!")
          (format t "This feed could not be deleted.")))
    ((string= *script-mode* "list") 
        ;; list all entries
        (let ((feedlist (list-all-feeds)))
          (if feedlist
            (progn
              ;; print a list of feeds
              (format t (concatenate 'string 
                (prin1-to-string (list-length feedlist))
                (if (eql 1 (list-length feedlist))
                  " feed is set up:~%"
                  " feeds are set up:~%")))
              (force-output nil)
              (print-feed-list feedlist))
            (format t "You don't have any feeds yet."))))
    ((string= *script-mode* "parse")
        ;; the parser for existing sites
        (parse-all-feeds))
    ((string= *script-mode* "webserver")
        ;; start the web server
        (start-webserver))
    (t
        ;; else ...
        (show-syntax))))



(rssparser)
