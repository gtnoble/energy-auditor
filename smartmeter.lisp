(in-package :smart-meter-texas)

(ql:quickload "alexandria")
(ql:quickload "drakma")
(ql:quickload "flexi-streams")
(ql:quickload "shasht")
(ql:quickload "parse-float")


(defparameter cookie-jar (make-instance 'drakma:cookie-jar))
(defparameter json-mime "application/json")

(defclass electric-meter ()
  ((esiid
     :initarg :esiid
     :initform (error "Must supply esiid")
     :reader esiid
     :documentation "ESIID meter ID") 
   (meter-number
     :initarg :meter-number
     :initform nil
     :reader meter-number
     :documentation "Meter number")))

(defclass meter-reading-session ()
  ((username
     :initarg :username
     :initform (error "Must supply username")
     :reader username
     :documentation "Smart meter texas login username")
   (password
     :initarg :password
     :initform (error "Must supply login password")
     :reader password
     :documentation "Smart meter texas login password")
   (session-token
     :initarg :session-token
     :initform (error "Must supply session token")
     :accessor session-token
     :documentation "Must supply session token")))

(defun parse-json (json-stream)
  (shasht:read-json 
    (flexi-streams:octets-to-string json-stream))
  )

(defmacro make-error-condition (condition-name slot-name message-string)
  `(define-condition ,condition-name (error) 
     ((,slot-name :initarg ,(intern (symbol-name slot-name) "KEYWORD") :reader ,slot-name))
     (:report (lambda (condition stream)
                (format stream ,message-string (,slot-name condition))))))

(make-error-condition user-not-found-error username "User ~a not found")
(make-error-condition invalid-token-error token "Invalid token: ~a")
(make-error-condition invalid-esiid-error error-message "~a")
(make-error-condition too-many-requests-error error-message "~a")
(define-condition reading-not-ready (error) ())

(define-condition invalid-password-error (error)
  ((password :initarg :password :reader password)) 
  (:report "Invalid Password"))

(defun get-session-token (username password)
  "Get a new session token for making smart meter requests"
  (let 
    ((url "https://smartmetertexas.com/api/user/authenticate"))
    (let 
      ((response (parse-json
                  (drakma:http-request url
                                       :method :post
                                       :parameters (list
                                                     '("rememberMe" . "true")
                                                     (cons "username" username)
                                                     (cons "password" password))
                                       :content-type json-mime
                                       :accept json-mime
                                       :cookie-jar cookie-jar))))
      (let ((error-message (gethash "errormessage" response)))
        (when (equal error-message "ERR-USR-USERNOTFOUND")
          (error 'user-not-found-error :username username))
        (when (equal error-message "ERR-USR-INVALIDPASSWORDERROR")
          (error 'invalid-password-error :password password)))
      (gethash "token" response))))

(defun start-session (username password)
  "Start a smart meter reading session"
  (make-instance 'meter-reading-session 
                 :username username 
                 :password password 
                 :session-token (get-session-token username password)))

(defun smart-meter-request (url meter token)
  "General function for interacting with Smart Meter Texas API"
  (let ((response (drakma:http-request url
                                       :method :post
                                       :parameters (list
                                                     (cons "ESIID" (esiid meter))
                                                     (cons "MeterNumber" (meter-number meter)))
                                       :content-type json-mime
                                       :accept json-mime
                                       :additional-headers (list (cons "Authorization"  
                                                                       (concatenate 'string "Bearer " token)))
                                       :cookie-jar cookie-jar)))
    (let ((parsed-response (parse-json response)))
      (let ((response-message (gethash "message" parsed-response))
            (response-data (gethash "data" parsed-response))) 
        (when (equal response-message "Invalid Token")
          (error 'invalid-token-error :token token))
        (let ((response-error-message (gethash "errorMessage" response-data))
              (response-error-code (gethash "errorCode" response-data))
              (response-error-key (string-downcase (gethash "errorKey" response-data))))
          (when response-error-code
            (cond ((equal response-error-key "esiid") 
                   (error 'invalid-esiid-error :error-message response-error-message))
                  (t (error "Unknown error ~a" response-error-message))))
          response-data)))))

(defun request-reading (meter token)
  "Request a smart meter reading"
  (let ((response-data (smart-meter-request 
                         "https://smartmetertexas.com/api/ondemandread"
                         meter
                         token)))
    (let ((request-status (gethash "RequestStatus" response-data)) 
          (status-code (gethash "statusCode" response-data))
          (status-reason (gethash "statusReason" response-data)))
      (let ((successful-response? (not (equal request-status "FLR")))) 
        (when (not successful-response?)
          (cond ((equal status-code "5031") (restart-case 
                                              (error 'too-many-requests-error 
                                                     :error-message status-reason)
                                              (try-requesting-reading-again () (request-reading meter token))))
                (t (error "Reading request failed: ~a" status-reason))))
        t))))

(defun fetch-reading (meter token)
  "Retrieve a completed smart meter reading"
  (let ((response-data (smart-meter-request
                         "https://smartmetertexas.com/api/usage/latestodrread"
                         meter
                         token)))
    (let ((status (gethash "odrstatus" response-data))
          (reading (parse-float:parse-float (gethash "odrread" response-data)))
          (response-message (gethash "responseMessage" response-data)))
      (let ((successful-reading? (equal status "COMPLETED"))) 
        (if (not successful-reading?)
            (restart-case 
              (if (equal status "PENDING")
                  (error 'reading-not-ready)
                  (error "Could not fetch reading: ~a" response-message)) 
              (try-fetching-again () (fetch-reading meter token)))
            reading)))))

(defun refresh-session (session)
  "Refresh smart meter session, useful when the token has expired"
  (setf (session-token session) 
        (get-session-token (username session) (password session))))

(defun read-meter (meter session)
  "High level smart meter reading function, performs requesting and fetching readings"
  (handler-case (progn (request-reading meter (session-token session))
                       (fetch-reading meter (session-token session))) 
    (invalid-token-error 
      () 
      (read-meter meter (refresh-session session)))))
