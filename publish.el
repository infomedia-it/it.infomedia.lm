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

(defun my-org-tufte-preprocess-sidenotes (backend)
  "Cerca sidenote inline e standard nel buffer e le salva in `my-sidenote-map`.
Ogni nota viene sostituita da un marker univoco §N:label§ nel buffer."
  (when (eq backend 'html)
    (setq my-sidenote-map (make-hash-table :test 'equal))
    (setq my-sidenote-counter 1)
    (save-excursion
      ;; Inline footnotes: [fn::Testo...]
      (goto-char (point-min))
      (while (re-search-forward "\\[fn::\\(.*?\\)\\]" nil t)
        (let* ((end (match-end 0)) ; salva subito la fine del match
               (text (match-string 1))
               (label (format "%d" my-sidenote-counter))
               (marker (format "§N:%s§" label))
               (html (org-export-string-as text 'html t)))
          (puthash marker
                   (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
<span class=\"sidenote\">%s</span>" label label html)
                   my-sidenote-map)
          (setq my-sidenote-counter (1+ my-sidenote-counter)))))
    ;; Fai lo stesso per le note standard
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\[fn:\\([^]]+\\)\\] *\\(.*\\)$" nil t)
        (let* ((label (match-string 1))
               (text (match-string 2))
               (marker (format "§N:%s§" label))
               (html (org-export-string-as text 'html t)))
          (puthash marker
                   (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
<span class=\"sidenote\">%s</span>" label label html)
                   my-sidenote-map)
          ;; Elimina la definizione nel testo
          )))))

(defun my-org-tufte-replace-sidenote-markers (html backend info)
  "Sostituisce tutti i marker §N:label§ con sidenote HTML Tufte."
  (when (eq backend 'html)
    (replace-regexp-in-string
     ;; Regex che trova footnote HTML standard con id "fnr.LABEL"
     "<sup><a id=\"fnr\\.\\([^\"]+\\)\"[^>]*>[^<]*</a></sup>"
     (lambda (match)
       (let* ((label (match-string 1 match))
              (id (format "§N:%s§" label))
              (text (gethash id my-sidenote-map)))
         (if text text id))) ;; fallback se label non trovata
     html)))


(add-hook 'org-export-before-processing-hook #'my-org-tufte-preprocess-sidenotes)

(add-to-list 'org-export-filter-final-output-functions #'my-org-tufte-replace-sidenote-markers)

;(setq org-export-filter-final-output-functions nil)

