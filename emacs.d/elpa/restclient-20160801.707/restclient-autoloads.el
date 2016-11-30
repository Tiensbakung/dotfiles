;;; restclient-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "restclient" "../../../../.emacs.d/elpa/restclient-20160801.707/restclient.el"
;;;;;;  "7a2419194189d84dd6d07f9d20a615e3")
;;; Generated autoloads from ../../../../.emacs.d/elpa/restclient-20160801.707/restclient.el

(autoload 'restclient-http-send-current "restclient" "\
Sends current request.
Optional argument RAW don't reformat response if t.
Optional argument STAY-IN-WINDOW do not move focus to response buffer if t.

\(fn &optional RAW STAY-IN-WINDOW)" t nil)

(autoload 'restclient-http-send-current-raw "restclient" "\
Sends current request and get raw result (no reformatting or syntax highlight of XML, JSON or images).

\(fn)" t nil)

(autoload 'restclient-http-send-current-stay-in-window "restclient" "\
Send current request and keep focus in request window.

\(fn)" t nil)

(autoload 'restclient-mode "restclient" "\
Turn on restclient mode.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/restclient-20160801.707/restclient-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/restclient-20160801.707/restclient.el")
;;;;;;  (22590 63933 237684 777000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; restclient-autoloads.el ends here
