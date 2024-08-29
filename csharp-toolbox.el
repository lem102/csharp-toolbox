;;; csharp-toolbox.el --- Utilities for csharp

;;; Commentary:
;; 

(require 'treesit)
(require 'xref)
(require 'dape)

;;; Code:

;; FIXME: follow-mode causes strange behaviour
(defun jacob-format-csharp-statement ()
  "Format the csharp statement at point."
  (interactive)
  (let* ((get-statement-bounds (lambda ()
                                 (let ((statement-node (jacob-csharp--get-parent-node "statement")))
                                   (cons (treesit-node-start statement-node)
                                         (treesit-node-end statement-node)))))
         (node-start (car (funcall get-statement-bounds))))
    (save-excursion
      (let* ((node-end (cdr (funcall get-statement-bounds)))
             (source-text (buffer-substring-no-properties node-start node-end)))
        (shell-command-on-region node-start
                                 node-end
                                 (format "csharpier-wrapper %s %d"
                                         (shell-quote-argument source-text)
                                         (save-excursion
                                           (goto-char node-start)
                                           (- 100
                                              (- (point)
                                                 (progn
                                                   (beginning-of-line)
                                                   (point))))))
                                 nil
                                 'no-mark)
        (goto-char node-start))
      (let* ((node-end (cdr (funcall get-statement-bounds)))
             (indentation-amount (save-excursion
                                   (goto-char node-start)
                                   (- (point)
                                      (progn
                                        (beginning-of-line)
                                        (point))))))
        (indent-code-rigidly node-start
                             node-end
                             indentation-amount)
        (goto-char node-start))
      (let* ((node-end (cdr (funcall get-statement-bounds))))
        (goto-char node-end)
        (delete-char 1)))))

(defun jacob-csharp-toggle-async ()
  "Toggle method at point async."
  (interactive)
  (let ((method-node
         (treesit-parent-until (treesit-node-at (point))
                               (lambda (node)
                                 (equal "method_declaration"
                                        (treesit-node-type node))))))
    (if (null method-node)
        (message "method not found")
      (let ((async-node
             (seq-first
              (treesit-query-capture method-node
                                     '(("async" @async))
                                     nil
                                     nil
                                     "NODE-ONLY")))
            (type-node
             ;; 4th method node child from the back is the method type
             (let ((children (treesit-node-children method-node)))
               (seq-elt children (- (length children) 4)))))
        (save-excursion
          (cond ((and async-node
                      (equal (treesit-node-text type-node) "Task"))
                 (goto-char (treesit-node-start async-node))
                 (search-forward "async Task")
                 (replace-match "void"))
                (async-node
                 (goto-char (treesit-node-start async-node))
                 (re-search-forward "async Task<.*>")
                 (replace-match (treesit-node-text
                                 (seq-first
                                  (treesit-query-capture type-node
                                                         '((type_argument_list
                                                            "<"
                                                            (_)
                                                            @arguments
                                                            ">"))
                                                         nil
                                                         nil
                                                         "NODE-ONLY")))))
                ((equal (treesit-node-text type-node) "void")
                 (goto-char (treesit-node-start type-node))
                 (delete-region (treesit-node-start type-node)
                                (treesit-node-end type-node))
                 (insert "async Task"))
                (t
                 (goto-char (treesit-node-start type-node))
                 (search-forward (treesit-node-text type-node))
                 (replace-match (format "async Task<%s>"
                                        (treesit-node-text type-node))))))))))

(defun jacob-csharp-highlight-statement ()
  "Select statement at point."
  (interactive)
  (let ((node (jacob-treesit-dominating-node-at-point "statement")))
    (goto-char (treesit-node-end node))
    (set-mark (treesit-node-start node))))

(defun jacob-csharp-debug-test (no-debug)
  "Debug test at point.

Run without debugging if NO-DEBUG is non-nil."
  (interactive "P")
  (let* ((test-name (let* ((test-node (jacob-treesit-dominating-node-at-point "method_declaration"))
                           (name-node (car (last (treesit-filter-child test-node
                                                                       (lambda (node)
                                                                         (string-match "identifier" (treesit-node-type node))))))))
                      (substring-no-properties (treesit-node-text name-node)))))
    (let ((default-directory (locate-dominating-file (file-name-directory (buffer-file-name (current-buffer)))
                                                     (lambda (dir)
                                                       (not (seq-empty-p (seq-filter (lambda (filename)
                                                                                       (string-match-p "csproj" filename))
                                                                                     (directory-files dir))))))))
      (compile (format "VSTEST_HOST_DEBUG=%s dotnet test --filter DisplayName~%s"
                       (if no-debug "0" "1")
                       test-name))
      (with-current-buffer "*compilation*"
        (add-hook 'compilation-filter-hook
                  (lambda ()
                    (let ((inserted-text (buffer-substring-no-properties compilation-filter-start (point))))
                      (when (string-match-p "Process Id:" inserted-text)
                        (goto-char compilation-filter-start)
                        (search-forward "Process Id: ")
                        (netcoredbg (format "netcoredbg --attach %s" (thing-at-point 'word 'no-properties))))))
                  nil
                  'local)))))

(defun jacob-csharp-debug-dll ()
  "Attach `netcoredbg' to process that corresponds to current csharp file."
  (interactive)
  (let ((process-name (file-name-sans-extension
                       (car
                        (directory-files
                         (locate-dominating-file
                          (file-name-directory (buffer-file-name (current-buffer)))
                          (lambda (dir)
                            (not (seq-empty-p (seq-filter (lambda (filename)
                                                            (string-match-p "csproj$" filename))
                                                          (directory-files dir))))))
                         nil
                         "csproj")))))
    (gud-gdb (format "netcoredbg --attach %s"
                     (seq-find (lambda (x)
                                 (string-match-p (format "%s$" process-name)
                                                 (cdr (assoc 'args
                                                             (process-attributes x)))))
                               (list-system-processes))))))

(defun jacob-csharp-debug-process ()
  "Choose process to debug."
  (interactive)
  (netcoredbg (format "netcoredbg --attach %s"
                      (let ((choices
                             (seq-map (lambda (x)
                                        (cons (file-name-nondirectory (alist-get 'args
                                                                                 x))
                                              (alist-get 'id
                                                         x)))
                                      (seq-filter (lambda (x)
                                                    ;; TODO: use project to work out regex
                                                    (string-match-p "/home/jacobl/dev/tappit/tappit-v2/"
                                                                    (alist-get 'args
                                                                               x)))
                                                  (seq-map (lambda (x)
                                                             (cons (cons 'id
                                                                         x)
                                                                   (process-attributes x)))
                                                           (list-system-processes))))))
                        (cdr (assoc-string (completing-read "Process to debug: "
                                                            choices)
                                           choices))))))

(defun tappit-process-id ()
  "Get process id of tappit program."
  (interactive)
  (let ((choices
         (seq-map (lambda (x)
                    (cons (file-name-nondirectory (alist-get 'args
                                                             x))
                          (alist-get 'id
                                     x)))
                  (seq-filter (lambda (x)
                                ;; TODO: use project to work out regex
                                (string-match-p "/home/jacobl/dev/tappit/tappit-v2/"
                                                (alist-get 'args
                                                           x)))
                              (seq-map (lambda (x)
                                         (cons (cons 'id
                                                     x)
                                               (process-attributes x)))
                                       (list-system-processes))))))
    (cdr (assoc-string (completing-read "Process to debug: "
                                        choices)
                       choices))))

(defun jacob-csharp-convert-namespace ()
  "Convert namespace block into namespace declaration."
  (interactive)
  (save-excursion
    (let ((namespace-declaration-node
           (treesit-parent-until (treesit-node-at (point))
                                 (lambda (node)
                                   (equal (treesit-node-type node)
                                          "namespace_declaration")))))
      (if (null namespace-declaration-node)
          (message "namespace declaration not found")
        (let* ((namespace-declaration-position
                (treesit-node-start namespace-declaration-node))
               (declaration-list-node
                (seq-find (lambda (node)
                            (equal (treesit-node-type node)
                                   "declaration_list"))
                          (treesit-node-children namespace-declaration-node)))
               (open-brace-position
                (treesit-node-start
                 (seq-first
                  (treesit-node-children
                   declaration-list-node)))))
          (goto-char open-brace-position)
          (forward-sexp)
          (delete-char -1)
          (goto-char open-brace-position)
          (delete-char 1)

          (goto-char namespace-declaration-position)
          (end-of-line)
          (insert ";")

          (forward-line 2)
          (indent-code-rigidly (point)
                               (buffer-end 1)
                               -4))))))

(defun jacob-csharp-synchronise-related-methods ()
  "Synchronise signature of \"related\" methods with the method at point.

Related meaning the interface for the method and the other
implementations."
  (interactive)
  (let ((method-node
         (treesit-parent-until (treesit-node-at (point))
                               (lambda (node)
                                 (string-equal "method_declaration"
                                               (treesit-node-type node))))))
    (if (null method-node)
        (message "no method at point")
      (let ((parameters
             (treesit-node-text
              (seq-first
               (treesit-query-capture method-node
                                      '((parameter_list
                                         "("
                                         _
                                         @parameters
                                         ")"))
                                      nil
                                      nil
                                      "NODE-ONLY"))))
            (references
             (let ((children
                    (treesit-node-children method-node)))
               (goto-char (treesit-node-start
                           (seq-elt children
                                    (- (length children)
                                       3))))
               (let ((xref-show-xrefs-function
                      (lambda (fetcher alist)
                        (seq-map 'xref-item-location
                                 (funcall fetcher)))))
                 (xref-find-references
                  (xref-backend-identifier-at-point
                   'eglot-xref-backend))))))

        ;; replace their parameter lists
        (seq-do (lambda (reference)
                  (find-file (xref-file-location-file reference))
                  (goto-char (point-min))
                  (forward-line (xref-file-location-line reference))
                  ;; (forward-char (xref-file-location-column reference))
                  ;; ah fuck
                  ;; i can't get the references to the other methods
                  ;; bugger
                  ;; this function won't work ;_;
                  ;; but i learnt some stuff i guess
                  )
                references)))))

(defun jacob-csharp-run-test (debug)
  "Run test at point.

Set the environment variable `VSTEST_HOST_DEBUG' to 1 if DEBUG is
non-nil."
  (interactive "P")
  (let* ((test-name
          (format "%s.%s.%s"
                  (treesit-node-text
                   (treesit-node-child-by-field-name
                    (jacob-csharp--get-namespace-node)
                    "name")
                   "NO_PROPERTY")
                  (treesit-node-text
                   (jacob-csharp--get-identifier-child
                    (jacob-csharp--get-class-node))
                   "NO_PROPERTY")
                  (treesit-node-text
                   (jacob-csharp--get-identifier-child
                    (jacob-csharp--get-method-node))
                   "NO_PROPERTY")))
         (default-directory
          (locate-dominating-file
           (file-name-directory (buffer-file-name (current-buffer)))
           (lambda (dir)
             (not (seq-empty-p (seq-filter
                                (lambda (filename)
                                  (string-match-p "csproj" filename))
                                (directory-files dir))))))))
    (compile (format "VSTEST_HOST_DEBUG=%d dotnet test --filter \"FullyQualifiedName=%s\""
                     (if debug 1 0)
                     test-name))
    (with-current-buffer "*compilation*"
      (let ((process-id))
        (add-hook 'compilation-filter-hook
                  (lambda ()
                    (let ((inserted-text
                           (buffer-substring-no-properties compilation-filter-start
                                                           (point))))
                      (when (string-match-p "Process Id:" inserted-text)
                        (goto-char compilation-filter-start)
                        (search-forward "Process Id: ")
                        (setq process-id (thing-at-point 'word "NO_PROPERTIES")))
                      (when (string-match-p "Waiting for debugger attach" inserted-text)
                        (let* ((dll
                                (jacob-select-dll))
                               (config
                                `(modes (csharp-mode csharp-ts-mode)
                                        ensure dape-ensure-command
                                        command "netcoredbg"
                                        command-args ["--interpreter=vscode"]
                                        :request "attach"
                                        :cwd ,(funcall dape-cwd-fn)
                                        :program ,dll
                                        :stopAtEntry t
                                        :processId ,process-id)))
                          (dape config)))))
                  nil
                  "LOCAL")))))

(defun jacob-csharp--get-namespace-node ()
  "Get the namespace node at point."
  (jacob-csharp--get-parent-node "\\(file_scoped_\\)?namespace_declaration"))

(defun jacob-csharp--get-class-node ()
  "Get the class node at point."
  (jacob-csharp--get-parent-node "class_declaration"))

(defun jacob-csharp--get-method-node ()
  "Get the method node at point."
  (jacob-csharp--get-parent-node "method_declaration"))

(defun jacob-csharp--get-parent-node (regexp)
  "Find dominating node at point that matches REGEXP."
  (treesit-parent-until (treesit-node-at (point))
                        (lambda (node)
                          (string-match-p regexp
                                          (treesit-node-type node)))
                        t))

(defun jacob-csharp--get-identifier-child (node)
  "Get the identifier node child of NODE.

Useful for getting the name node of classes or methods."
  (car
   (last
    (treesit-filter-child node
                          (lambda (node)
                            (string-match "identifier"
                                          (treesit-node-type node)))))))

;; JACOBTODO: try make a function for creating fields and adding them to the constructor

(defun jacob-csharp-rename-file ()
  "Rename the current file and class/whatever."
  (interactive)
  (let ((new-name (read-from-minibuffer "Rename window/class to: "))
        (class-name-range (seq-first (treesit-query-range (treesit-buffer-root-node)
                                                          '((interface_declaration (identifier) @identifier))))))
    (rename-visited-file (format "%s.cs" new-name))
    (delete-region (car class-name-range) (cdr class-name-range))
    (goto-char (car class-name-range))
    (insert new-name)))

(provide 'csharp-toolbox)

;;; csharp-toolbox.el ends here
