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

;; (defvar my-sidenote-map nil)
(defvar my-sidenote-map (make-hash-table :test 'equal))


(defun my-org-tufte-remove-footnote-block (backend)
  "Elimina le footnote standard alla fine dell’articolo."
  (when (eq backend 'html)
    (org-element-map data 'footnote-definition
      (lambda (fn) (org-element-extract-element fn))))
  data)

(defun my-org-tufte-collect-footnotes (backend)
  "Cattura le definizioni delle footnote in `my-sidenote-map` durante l'export Org → HTML."
  (when (eq backend 'html)
    (setq my-sidenote-map (make-hash-table :test 'equal)
          my-sidenote-counter 1)
    (org-element-map data 'footnote-definition
      (lambda (fn)
        (let* ((label (org-element-property :label fn))
               (contents (org-export-data (org-element-contents fn) info))
               (id (format "sn-%s" label)))
          (puthash id contents my-sidenote-map)))
      info))
  data)

(defun my-org-tufte-sidenote-html-filter (html backend info)
  "Sostituisce le note inline HTML con markup Tufte CSS."
  (when (eq backend 'html)
    (let ((pos 0)
          (result "")
          (pattern "<sup><a\\([^>]+\\)id=\"fnr\\.\\([^\"]+\\)\"\\([^>]*\\)>\\([^<]*\\)</a></sup>"))
      (while (string-match pattern html pos)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (label (match-string 2 html))
               (id (format "sn-%s" label))
               (text (gethash id my-sidenote-map))
               (html-text (and text (org-export-string-as text 'html t))))
          (setq result
                (concat result
                        (substring html pos start)
                        (if html-text
                            (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
<span class=\"sidenote\">%s</span>" id id html-text)
                          (substring html start end))))
          (setq pos end)))
      (concat result (substring html pos)))))


(add-hook 'org-export-before-processing-functions #'my-org-tufte-collect-footnotes)
(add-hook 'org-export-filter-final-output-functions #'my-org-tufte-sidenote-html-filter)
(add-hook 'org-export-before-processing-functions #'my-org-tufte-remove-footnote-block)

