;;;; model.lisp
 
(in-package #:compost)

;;; INITIALIZATION

(defparameter +data-store-directory-name+
  "compost-store")

(defvar *tag-index* NIL
  "A hashtable that indexes lists of posts by keyword tags.")

(defvar *active-invites* NIL
  "A hashtable that keeps active invites.")

(defun data-store-path ()
  (make-pathname
   :directory (append (pathname-directory (user-homedir-pathname))
                      (list +data-store-directory-name+))))

(defun initialize-datastore ()
  (ensure-directories-exist (data-store-path))
  (make-instance
   'db:mp-store
   :directory (data-store-path)
   :subsystems (list (make-instance 'db:store-object-subsystem)
                     (make-instance 'db:blob-subsystem))))

;;; CLASSES

(defclass topic (db:store-object)
  ((name
    :accessor topic-name
    :initarg :name
    :initform (error "topics must have a name")
    :index-type bknr.indices:string-unique-index
    :index-reader topic-by-name
    :index-values all-topics))
  (:metaclass db:persistent-class))

(defclass user (db:store-object)
  ((name
    :accessor user-name
    :initarg :name
    :initform (error "USERs must have a name")
    :index-type bknr.indices:string-unique-index 
    :index-reader user-by-name
    :index-values all-users)
   (pw-hash
    :accessor pw-hash
    :initarg :pw
    :initform (error "USERs must have a password"))
   (timezone
    :accessor user-timezone
    :initform "US/Central"))
  (:metaclass db:persistent-class ))


(defclass session (db:store-object)
  ((user
    :reader session-user
    :initarg :user)
   (cookie
    :reader session-cookie
    :initarg :cookie
    :index-type bknr.indices:string-unique-index
    :index-reader session-by-cookie))
  (:metaclass db:persistent-class))

(defclass post (db:store-object)
  ((created
    :reader post-created
    :initform (get-universal-time))
   (text
    :accessor post-text
    :initarg :text
    :initform "")
   (render-style
    :accessor post-render-style
    :initarg :render-style
    :initform :markdown)
   (user
    :reader post-user
    :initarg :user
    :initform (error "Posts must have a user")
    :index-type bknr.indices:hash-index
    :index-reader posts-by-user)
   )
  (:metaclass db:persistent-class))

(defclass attachment (db:blob)
  ((filename
    :reader attachment-filename
    :initarg :filename
    :initform (error "Attachments must have a name"))
   (post
    :reader attachment-post
    :initarg :post
    :initform (error "Attachments must have a post")
    :index-type bknr.indices:hash-index
    :index-reader post-attachments))
  (:metaclass db:persistent-class))

(defclass title-post (post)
  ((title
    :accessor post-title
    :initarg :title
    :initform (error "Title posts must have a title"))
   (topic
    :accessor post-topic
    :initarg :topic
    :initform nil
    :index-type bknr.indices:hash-index
    :index-reader posts-by-topic)
   (status
    :accessor post-status
    :initarg :status
    :initform :posted
    :index-type bknr.indices:hash-index
    :index-reader posts-by-status)
   (tags
    :accessor post-tags
    :initarg :tags
    :initform (list)
    :documentation "A List of keywords used to tag this post")   )
  (:metaclass db:persistent-class))

(defclass reply-post (post)
  ((predecessor
    :accessor reply-to
    :initarg :predecessor
    :initform (error "reply posts require a predecessor")
    :index-type bknr.indices:hash-index
    :index-reader replies-to))
  (:metaclass db:persistent-class))


;;; MODEL FUNCTIONS

(defun make-user (username password)
  (when (user-by-name (string-downcase username))
    (error "A user already exists named ~a" username))
  (make-instance 'user
                 :name (string-downcase username)
                 :pw (pw-digest password)))

(defun make-session (user)
  (make-instance 'session
                 :user user
                 :cookie (make-uid (user-name user))))

(defun cookie-header-value (session)
  (format nil "~a=~a" +session-cookie-key+
          (session-cookie session)))

(defun login-user (username password)
  "Looks up a PLAYER by username and password."
  (when-let (user (user-by-name (string-downcase username)))
    (when (equal (pw-digest password) (pw-hash user))
      user)))

(defun render-post (post)
  "Renders the text of POST to SPINNERET:*HTML* and returns no values"
  (with-slots (text render-style) post
    (let ((renderer
            (case render-style
              ((:md :markdown)  'markdown:parse)
              (t 'identity))))
      (princ (funcall renderer text) spinneret:*html*  )
      (values))))

(defgeneric path-to (object)
  (:documentation "Returns a url path to the object"))

(defmethod path-to ((topic topic))
  (format nil "/topic/view/~a" (topic-name topic)))

(defmethod path-to ((attachment attachment))
  (format nil "/file/~a/~a"
          (db:store-object-id attachment)
          (attachment-filename attachment)))

(defmethod path-to ((post title-post))
  (format nil "/post/view/~a"
          (db:store-object-id post)))

(defmethod path-to ((post reply-post))
  (format nil
          "~a#comment-~a"
          (path-to (find-root-post post))
          (db:store-object-id post)))


(defgeneric find-root-post (post)
  (:documentation "Navigages up the reply tree to the root post"))

(defmethod find-root-post ((post title-post)) post)
(defmethod find-root-post ((post reply-post))
  (find-root-post (reply-to post)))

(defmethod sorted-replies-to (post)
  (sort (copy-seq (replies-to post))
        #'<
        :key #'post-created))

(defun most-recent-posts (&key (count 10))
  (let ((posts (sort (copy-seq (db:store-objects-with-class 'post))
                #'>
                :key 'post-created)))
    (subseq posts  0 (min count (length posts)))))


(defun tag-post (post tag)
  (assert (keywordp tag))
  (setf (gethash tag *tag-index*)
        (pushnew post (gethash tag *tag-index*))))

(defun untag-post (post tag)
  (assert (keywordp tag))
  (setf (gethash tag *tag-index*)
        (remove post (gethash tag *tag-index*))))

(defun initialize-invite-tab ()
  "Initialize invite table. Call again to reinitialize."
  (setf *active-invites* (make-hash-table :synchronized t :test 'equal)))

(defun initialize-tag-index ()
  "Initializes the tag index. Call again to reinitialize."
  (setf *tag-index* (make-hash-table :synchronized t))
  (dolist (post (db:store-objects-with-class 'title-post))
    (dolist (tag (post-tags post))
      (tag-post post tag))))

(defun posts-by-tag (tag1 &rest moretags)
  "Return a list of posts tagged with all supplied tags.  Tags are keywords."
  (when-let (tagged (gethash tag1 *tag-index*))
    (dolist (tag moretags tagged)
      (setf tagged
            (intersection tagged
                          (gethash tag *tag-index*))))))


(defun make-invite (user)
  (declare (ignore user))
  (let ((invite-key (make-uid "invitation")))
    (setf (gethash invite-key *active-invites*) :unclaimed)
    invite-key))
