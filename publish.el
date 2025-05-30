(require 'ox-publish)

(setq org-publish-project-alist
      '(("it.infomedia.lm"
         :base-directory "~/Dropbox/LIFE/10-projects/10.50 infomedia site/it.infomedia.lm/org"
         :publishing-directory "~/Dropbox/LIFE/10-projects/10.50 infomedia site/it.infomedia.lm/html"
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author t
         :with-creator t
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\" href=\"../css/tufte.css\" />"
         :html-html5-fancy t
         :html-doctype "html5")))

(org-publish-all t)

(defun it.infomedia.lm-publish (do-push)
  "Esegue org-publish e make publish.
Se chiamata con argomento prefisso (C-u), esegue anche git commit e push."
  (interactive "P")
  (let* ((project-name "it.infomedia.lm")
         (project (assoc project-name org-publish-project-alist))
         (base-dir (expand-file-name (plist-get (cdr project) :base-directory))))
    (unless (and base-dir (file-directory-p base-dir))
      (error "Directory base non trovata per il progetto %s" project-name))
    (let ((default-directory (file-name-directory (directory-file-name base-dir))))
      (setq my-sidenote-counter 1)
      (message "📤 Export Org → HTML…")
      (org-publish-project project-name t)
      (message "🛠  make publish…")
      (shell-command "make publish")
      (unless do-push
        (message "🚀 commit + push…")
        (shell-command "git commit -a -m 'step' && git push"))
      (message "✅ Pubblicazione completata."))))


(defvar-local my-sidenote-counter 1)

(defun my-org-tufte-sidenote-id (footnote)
  "Return a unique sidenote ID based on FOOTNOTE object."
  (cond
   ;; Etichetta presente
   ((and (listp footnote)
         (stringp (car footnote)))
    (format "sn-%s" (car footnote)))
   ;; Inline o anonima
   (t
    (let ((n my-sidenote-counter))
      (setq my-sidenote-counter (1+ n))
      (format "sn-%d" n)))))

(defun my-org-tufte-sidenote-content (footnote info)
  "Return the HTML content of the sidenote."
  (cond
   ;; Footnote inline semplice (stringa)
   ((stringp footnote)
    (org-export-string-as footnote 'html t))
   ;; Etichettata inline (forma [fn:label:contenuto])
   ((and (listp footnote)
         (car footnote)           ; label
         (stringp (cdr footnote))) ; contenuto inline
    (org-expor
