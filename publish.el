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


(defvar my-sidenote-counter 1
  "Contatore globale per sidenote Tufte.")

(defvar my-sidenote-map nil)

(defun my-org-collect-sidenotes (_backend)
  "Crea una mappa delle sidenotes inline prima dell'export."
  (setq my-sidenote-counter 1)
  (setq my-sidenote-map
        (let ((map (make-hash-table :test 'equal)))
          (org-element-map (org-element-parse-buffer) 'footnote-reference
            (lambda (fn)
              (when (eq (org-element-property :type fn) 'inline)
                (let* ((label (or (org-element-property :label fn)
                                  (format "sn-%d" my-sidenote-counter)))
                       (contents (org-element-interpret-data (org-element-contents fn))))
                  (puthash label contents map)
                  (setq my-sidenote-counter (1+ my-sidenote-counter))))))
          map)))

(defun my-org-tufte-sidenote-html-filter (html backend info)
  "Sostituisce l'HTML di note inline con markup Tufte."
  (when (eq backend 'html)
    (replace-regexp-in-string
     ;; Regex che trova footnote HTML standard con id "fnr.LABEL"
     "<sup><a id=\"fnr\\.\\([^\"]+\\)\"[^>]*>[^<]*</a></sup>"
     (lambda (match)
       (let* ((label (match-string 1 match))
              (id (format "sn-%s" label))
              (text (gethash id my-sidenote-map)))
         (if text
             (format "
<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
<span class=\"sidenote\">%s</span>" id id text)
           match))) ;; fallback se label non trovata
     html)))

(add-hook 'org-export-before-processing-functions #'my-org-collect-sidenotes)
(add-to-list 'org-export-filter-final-output-functions #'my-org-tufte-sidenote-html-filter)
