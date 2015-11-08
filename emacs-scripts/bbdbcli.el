#!/usr/bin/emacs --script

(package-initialize)
(require 'bbdb)
(require 'bbdb-com)
(require 'json)
(require 'cl)
(require 'commander)

(defun bbdbcli-create (args)
  (message "create!")
)

(defun bbdbcli-delete (args)
  (message "delete!")
)

(defun bbdbcli-update (args)
  (message "update!")
)

(defun bbdbcli-fetch (args)
  (message "fetching contact from BBDB with text: %s" (elt args 0))
  (fetch-contacts-json-batch (elt args 0))
)

(defun fetch-contacts-json-batch (text)
  (princ (fetch-contacts-json text)))
  
(defun fetch-contacts-json (text)
  (bbdb text)
  (save-current-buffer
    (set-buffer "*BBDB*")
    (json-encode
     (apply 'vector
            (mapcar 'bbdb-record-to-contact
                    (mapcar 'car bbdb-records))))))

(defun bbdb-record-to-contact (record)
  (let ((fields '((:hash woe-record-hash)
                  (:firstName woe-record-firstname)
                  (:lastName woe-record-lastname)
                  (:email woe-record-mail)
                  (:organization woe-record-organization)
                  (:telephone woe-record-telephone)
                  (:address woe-record-address))))
    (contact-internal fields record)))

(defun contact-internal (field-list record)
  "convert a bbdb record into the list of fields given"
  (if (eq field-list 'nil)
      'nil
    (let ((field-value (funcall (cadar field-list) record)))
      (if (eq field-value 'nil)
          (contact-internal (cdr field-list) record)
        (append (list (caar field-list) field-value)
                (contact-internal (cdr field-list) record))))))
  
(defun woe-record-firstname(record)
  (bbdb-record-firstname record))

(defun woe-record-lastname(record)
  (bbdb-record-lastname record))

(defun woe-record-mail(record)
  (apply 'vector (bbdb-record-mail record)))

(defun woe-record-organization(record)
  (apply 'vector (bbdb-record-organization record)))

(defun woe-record-telephone(record)
  (apply 'vector (mapcar 'bbdb-phone-to-woe (bbdb-record-phone record))))

(defun woe-record-address(record)
  (apply 'vector (mapcar 'bbdb-address-to-woe (bbdb-record-address record))))

(defun bbdb-phone-to-woe (bbdb-phone)
  "Converts a bbdb phone vector into a JSON friendly plist"
  (list ':label (bbdb-phone-label bbdb-phone)
        ':areaCode (bbdb-phone-area bbdb-phone)
        ':prefix (bbdb-phone-exchange bbdb-phone)
        ':suffix (bbdb-phone-suffix bbdb-phone)
        ':extension (bbdb-phone-extension bbdb-phone)))

(defun bbdb-address-to-woe (bbdb-address)
  "Converts a bbdb address vector into a JSON friendly plist"
  (list ':label (bbdb-address-label bbdb-address)
        ':streetLines (apply 'vector (bbdb-address-streets bbdb-address))
        ':city (bbdb-address-city bbdb-address)
        ':state (bbdb-address-state bbdb-address)
        ':postalCode (bbdb-address-postcode bbdb-address)
        ':country (bbdb-address-country bbdb-address)))

(defun woe-record-hash (record)
  (secure-hash 'md5 (prin1-to-string (list (bbdb-record-firstname record)
                                           (bbdb-record-lastname record)
                                           (bbdb-record-mail record)
                                           (bbdb-record-organization record)
                                           (bbdb-record-phone record)
                                           (bbdb-record-address record)))))

(defun main ()
  "run the main program"
  (let ((action (car argv)) (args (cdr argv)))
    (cond
     ((equal action "create") (bbdbcli-create args))
     ((equal action "delete") (bbdbcli-delete args))
     ((equal action "update") (bbdbcli-update args))
     ((equal action "fetch") (bbdbcli-fetch args))
     ('t (message "unknown action %s" action)))))

(main)
