(asdf:load-system :completions)
(asdf:load-system :dexador)
(asdf:load-system :jonathan)
(asdf:load-system :split-sequence)
(asdf:load-system :clingon)

(defpackage :gail
  (:use :cl :split-sequence)
  (:export #:main))
(in-package :gail)

(defun get-github-token ()
  (or (uiop:getenv "GITHUB_TOKEN")
      (error "GITHUB_TOKEN environment variable is not set.")))

(defun get-openai-key ()
  (or (uiop:getenv "OPENAI_KEY")
      (error "OPENAI_KEY environment variable is not set.")))

(defun read-labels-from-file (filename)
  "Read labels from a text file, one label per line, ignoring empty lines and comments."
  (when (probe-file filename)
    (with-open-file (stream filename :direction :input)
      (loop for line = (read-line stream nil nil)
            while line
            for trimmed = (string-trim '(#\Space #\Tab) line)
            when (and (> (length trimmed) 0)
                      (not (char= (char trimmed 0) #\#)))
            collect (string-upcase trimmed)))))

(defun attach-labels (owner repo issue labels token)
  "Attach LABELS (list of strings) to GitHub ISSUE in OWNER/REPO using TOKEN via dexador:post."
  (let* ((url     (format nil "https://api.github.com/repos/~a/~a/issues/~a/labels"
                         owner repo issue))
         (auth    (format nil "token ~a" token))
         (payload (jonathan:to-json labels)))
    (dexador:post url
      :headers `(("Authorization"   . ,auth)
                 ("Content-Type"    . "application/json")
                 ("Accept"          . "application/vnd.github.v3+json"))
      :content payload)))

(defun fetch-all-issues-raw (owner repo)
  "Returns a list of JSON strings representing open issues from OWNER/REPO."
  (let ((base-url (format nil "https://api.github.com/repos/~a/~a/issues" owner repo))
        (token (get-github-token))
        (page 1)
        (per-page 100)
        (results '()))
    (loop
       for url = (format nil "~A?state=open&per_page=~D&page=~D" base-url per-page page)
       for response = (dex:get url
                               :headers `(("Authorization" . ,(format nil "token ~A" token))
                                          ("User-Agent" . "issue-labeler")))
       do (push response results)
       while (> (length response) 10) ;; crude check: if the page is empty, GitHub returns "[]"
       do (incf page))
    (nreverse results)))

(defun classify-json-with-ai (completer json-string available-labels)
  (let* ((labels-string (format nil "~{~A~^, ~}" available-labels))
         (prompt (format nil
                 "Examine the following JSON object representing a GitHub issue. \
Respond with the Issue Number followed by a space-separated list of labels that are appropriate for this issue. \
Here are your label choices: ~A.
~%~%If none of the label choices are appropriate for the issue, don't list any. \
Don't say anything else.\n\n~A\n\nAnswer:"
                 labels-string json-string)))
    (handler-case
        (string-trim " \n" (completions:get-completion completer prompt :max-tokens 16384))
      (dexador.error:http-request-bad-request (e)
        (format t "~&HTTP error: ~A~%" e)
        (when (slot-boundp e 'dexador.error::response)
          (let ((response (slot-value e 'dexador.error::response)))
            (format t "Response body: ~A~%" response)))
        (uiop:quit)))))

(defun label-issues (json-pages completer owner repo available-labels &key dry-run)
  (let ((token (get-github-token)))
    (dolist (page json-pages)
      (let ((issues (jonathan:parse page)))
        (dolist (issue issues)
          ;; re-encode individual issue as JSON string for prompting
          (let ((issue-json (jonathan:to-json issue)))
            (let ((result (classify-json-with-ai completer issue-json available-labels)))
              (let* ((trimmed-result (string-trim '(#\Space #\Tab #\Newline) result)))
                (when (> (length trimmed-result) 0)
                  (let* ((parts  (split-sequence #\Space trimmed-result :remove-empty-subseqs t))
                         (issue-num  (first parts))
                         (labels (map 'list #'string-downcase (rest parts))))
                    (when (and issue-num labels)
                      (format t "Issue ~a would be labeled with: ~a~%" issue-num labels)
                      (unless dry-run
                        (handler-case
                            (progn
                              (attach-labels owner repo issue-num labels token)
                              (format t "  Success!~%"))
                          (error (e)
                            (format t "  Error on issue ~a: ~a~%" issue-num e)))))))))))))))

(defun gail/options ()
  "Define command-line options for gail"
  (list
   (clingon:make-option
    :filepath
    :description "Labels file"
    :short-name #\l
    :long-name "labels"
    :initial-value ".gail-labels"
    :key :labels-file)

   (clingon:make-option
    :flag
    :description "Show what would be labeled without actually labeling"
    :short-name #\n
    :long-name "dry-run"
    :key :dry-run)

   (clingon:make-option
    :string
    :description "OpenAI model to use"
    :short-name #\m
    :long-name "model"
    :initial-value "gpt-4o-mini"
    :key :model)))

(defun gail/handler (cmd)
  "Main command handler for gail"
  (let ((labels-file (clingon:getopt cmd :labels-file))
        (dry-run (clingon:getopt cmd :dry-run))
        (model (clingon:getopt cmd :model))
        (args (clingon:command-arguments cmd)))

    ;; Validate positional arguments
    (when (not (eq (length args) 2))
      (format *error-output* "Error: OWNER and REPO arguments are required~%~%")
      (clingon:print-usage-and-exit cmd t))

    (let ((owner (first args))
          (repo (second args)))

      ;; Read labels from file
      (let ((available-labels (read-labels-from-file labels-file)))
        (unless available-labels
          (format *error-output* "Error: Could not read labels from ~a or file is empty~%" labels-file)
          (uiop:quit 1))

        (format t "Using labels from ~a: ~{~A~^, ~}~%" labels-file available-labels)
        (format t "Repository: ~a/~a~%" owner repo)
        (format t "Model: ~a~%" model)
        (when dry-run
          (format t "DRY RUN MODE - No labels will actually be applied~%"))
        (format t "~%")

        ;; Fetch issues and process them
        (format t "Fetching issues from ~a/~a...~%" owner repo)
        (let ((json-pages (fetch-all-issues-raw owner repo))
              (completer (make-instance 'completions:openai-completer
                                        :api-key (get-openai-key)
                                        :model model)))
          (format t "Processing ~d page(s) of issues...~%" (length json-pages))
          (label-issues json-pages completer owner repo available-labels :dry-run dry-run))

        (format t "~&Done.~%")))))

(defun gail/command ()
  "Create the main gail command"
  (clingon:make-command
   :name "gail"
   :version "1.0"
   :description "GitHub Issue Auto-Labeler using AI"
   :authors '("Anthony Green <green@moxielogic.com>")
   :license "MIT License"
   :usage "OWNER REPO"
   :options (gail/options)
   :handler #'gail/handler
   :examples '(("Label issues in the libffi/libffi repository:"
                . "gail libffi libffi")
               ("Dry run with custom labels file:"
                . "gail --labels my-labels.txt --dry-run microsoft vscode")
               ("Use a different OpenAI model:"
                . "gail --model gpt-4 owner repo"))))

(defun main ()
  "Main entry point"
  (handler-case
      (let ((app (gail/command)))
        (clingon:run app))
    (error (e)
      (format *error-output* "Error: ~a~%" e)
      (uiop:quit 1))))
