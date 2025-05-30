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

(defun my-org-tufte-sidenote-filter (footnote backend info)
  "Filtro Org Export per trasformare note inline in sidenotes Tufte.
Si applica solo all'export HTML. `transcoded` è il markup standard già generato."
  (when (and (eq backend 'html)
             (org-element-property :type footnote)
              (eq (org-element-property :type footnote) 'inline))
    (let* ((label (or (org-element-property :label footnote)
                      (format "sn-%d" my-sidenote-counter)))
              (id (format "sn-%s" label))
           ;; Estrae il contenuto della nota (lista di oggetti Org)
           (contents (org-export-data (org-element-contents footnote) info))
           (html (format "
<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
<span class=\"sidenote\">%s</span>" id id contents)))
      (setq my-sidenote-counter (1+ my-sidenote-counter))
      html)))

(defun my-reset-sidenote-counter (&rest _)
  "Resetta il contatore sidenote prima dell’export."
  (setq my-sidenote-counter 1))


(add-to-list 'org-export-filter-footnote-reference-functions
             #'my-org-tufte-sidenote-filter)

(add-hook 'org-export-before-processing-functions
          #'my-reset-sidenote-counter)
