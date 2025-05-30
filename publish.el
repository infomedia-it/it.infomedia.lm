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
  "Contatore globale per le sidenote.")


(defun my-org-tufte-sidenote-content (footnote info)
  "Esporta il contenuto della sidenote da un footnote OrgMode."
  (cond
   ;; Footnote inline: [fn:: ...]
   ((stringp footnote)
    (org-export-string-as footnote 'html t))
   ;; Footnote [fn:label: ...] → cdr è stringa
   ((and (listp footnote)
         (stringp (cdr footnote)))
    (org-export-string-as (cdr footnote) 'html t))
   ;; Footnote [fn:label] → cdr è un elemento Org (AST)
   ((and (listp footnote)
         (consp (cdr footnote)))
    (condition-case nil
        (org-export-data (cdr footnote) info)
      (error (format "<!-- ERRORE: %S -->" (cdr footnote)))))
   (t
    (format "<!-- nota non riconosciuta: %S -->" footnote))))

(defun my-org-tufte-sidenote (footnote info)
  "Restituisce il markup HTML per una nota a margine Tufte."
  (let* ((label (or (and (listp footnote) (car footnote))
                    (format "sn-%d" my-sidenote-counter)))
         (id (format "sn-%s" label))
         (html (my-org-tufte-sidenote-content footnote info)))
    (setq my-sidenote-counter (1+ my-sidenote-counter))
    (format
     "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/>
<span class=\"sidenote\">%s</span>" id id html)))

(defun my-org-html-footnote-reference (footnote contents info)
  "Override per generare sidenote Tufte invece di note classiche."
  (if (or (stringp footnote)
          (and (listp footnote)
               (or (cdr footnote)
                   (and (stringp (cdr footnote))))))
      (my-org-tufte-sidenote footnote info)
    (org-html-format-footnote-reference footnote info)))

(defun my-disable-html-footnote-section (_info)
  "Rimuove la sezione delle note a piè di pagina."
  "")

(advice-add 'org-html-footnote-reference :override #'my-org-html-footnote-reference)
(advice-add 'org-html-footnote-section :override #'my-disable-html-footnote-section)

