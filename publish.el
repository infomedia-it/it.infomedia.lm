(require 'ox-publish)
(require 'mustache)

(setq debug-on-error t)

(org-export-define-derived-backend 'html-tufte 'html
  :translate-alist '((template . exedre/html-template-from-file)))

(defun exedre/html-template-from-file (contents info)
  "Legge un file HTML e sostituisce variabili come {{title}}, poi inserisce CONTENTS."
  (let* ((plist info)  ;; ← questo c'è sempre
         (template-name (or (plist-get plist :template-name) "default"))
         (base-directory (file-name-directory
                          (directory-file-name
                           (plist-get plist :base-directory))))
         (template-path (expand-file-name
                         (format "themes/%s/base.html" template-name)
                         base-directory)))
    (unless (file-readable-p template-path)
      (error "⚠️ Template file non trovato: %s" template-path))
    (let ((template (with-temp-buffer
                      (insert-file-contents template-path)
                      (buffer-string)))
          (vars `(("title" . ,(org-export-data (plist-get info :title) info))
                  ("author" . ,(org-export-data (plist-get info :author) info))
                  ("journal" . ,(or (plist-get info :journal) "e-privacy"))
                  ("volume" . ,(or (plist-get info :volume) ""))
                  ("edition" . ,(or (plist-get info :edition) ""))
                  ("subtitle" . ,(or (plist-get info :subtitle) "La vita è tutto un dossier"))
                  ("content" . ,contents))))
      (mustache-render template vars))))


(defun exedre/publishing-function (plist filename pub-dir)
  (let ((org-export-current-backend 'html-tufte))
    (org-publish-org-to 'html-tufte filename ".html" plist pub-dir)))


(setq org-publish-project-alist
      '(("it.infomedia.lm"
         :base-directory "~/Dropbox/LIFE/10-projects/10.50 infomedia site/it.infomedia.lm/org"
         :publishing-directory "~/Dropbox/LIFE/10-projects/10.50 infomedia site/it.infomedia.lm/html"
         :recursive t
         ;;:publishing-function org-html-publish-to-html
         :publishing-function exedre/publishing-function
         :with-author nil
         :with-creator nil
         :with-date nil
         :section-numbers nil
         :with-toc nil
         :html-html5-fancy t
         :html-validation-link nil
         :html-postamble nil
         :html-doctype "html5"
         :template-name tufte-theme
)))


(setq org-html-validation-link nil)

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
      (setq my-sidenote-replacements nil)
      (setq my-sidenote-list nil)
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

(defvar my-sidenote-list nil)

(defvar my-sidenote-replacements nil
  "Lista delle coppie (MATCH . REPLACEMENT) trovate da `my-org-tufte-replace-sidenote-markers`.")

(defun string-trim+ (s)
  "Rimuove spazi bianchi e tag <p>...</p> attorno a S, se presenti."
  (replace-regexp-in-string
   "\\`[ \t\n]*<p>[ \t\n]*\\(\\(?:.\\|\n\\)*?\\)[ \t\n]*</p>[ \t\n]*\\'"
   "\\1"
   s))


(defun my-org-footnote-occurrences ()
  "Restituisce una lista di tutte le occorrenze di note nel buffer Org.
Ogni occorrenza include:
  :label   → etichetta della nota (o nil per note inline anonime)
  :index   → numero progressivo di apparizione
  :content → testo della nota (inline o risolto da definizione)"
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (defs (make-hash-table :test #'equal))
         (counter 0)
         (results '()))
    ;; 1. Costruisci una tabella delle definizioni etichettate
    (org-element-map ast 'footnote-definition
      (lambda (fn)
        (puthash (org-element-property :label fn)
                 (org-element-interpret-data (org-element-contents fn))
                 defs)))
    ;; 2. Raccogli tutte le occorrenze
    (org-element-map ast 'footnote-reference
      (lambda (ref)
        (let* ((label (org-element-property :label ref))
               (inline (org-element-contents ref))
               (content
                (cond
                 ;; caso inline
                 ((and (not label) inline)
                  (org-element-interpret-data inline))
                 ;; caso label + inline (raro)
                 ((and label inline)
                  (org-element-interpret-data inline))
                 ;; caso label senza inline → cerca nella tabella
                 ((and label (gethash label defs))
                  (gethash label defs))
                 ;; etichetta non risolta
                 (label (format "[NO DEFINITION for %s]" label))
                 ;; caso fallback
                 (t "[UNRESOLVED]"))))
          (setq counter (1+ counter))
          (push `(:label ,label :index ,counter :content ,content)
                results))))
    (reverse results)))

(defun my-org-print-footnote-occurrences ()
  "Stampa tutte le occorrenze di note nel buffer corrente Org."
  (interactive)
  (let ((notes (my-org-footnote-occurrences)))
    (with-output-to-temp-buffer "*Footnotes*"
      (dolist (n notes)
        (princ (format "[%d] %s%s\n\n"
                       (plist-get n :index)
                       (if (plist-get n :label)
                           (format "[fn:%s] " (plist-get n :label))
                         "[fn::] ")
                       (plist-get n :content)))))))

(defun my-org-tufte-export-sidenote (id content)
    (format "<label for=\"sn-%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"sn-%s\" class=\"margin-toggle\">
<span class=\"sidenote\">%s</span>" id id  content))

(defun my-org-export-org-to-html (org-text)
"Converte una stringa ORG in HTML, disabilitando i filtri globali temporaneamente."
(save-excursion
  (let ((org-export-filter-final-output-functions nil)
        (org-export-before-processing-hook nil) 
        (org-export-filter-plain-text-functions nil)
        (org-export-filter-paragraph-functions nil))
       (string-trim+
        (org-export-string-as org-text 'html t
                              '(:with-footnotes nil))))))

(defun my-org-apply-sidenote-replacements (html backend info)
  "Applica le sostituzioni raccolte in `my-sidenote-replacements` su HTML."
    (when (eq backend 'html-tufte)
      (dolist (pair my-sidenote-replacements (or html ""))
        (setq html (replace-regexp-in-string
                    (regexp-quote (car pair)) (cdr pair) html t t)))))

(defun my-org-subst-match (_match)
"Salva il MATCH e il suo replacement dalla lista `my-sidenote-list` in `my-sidenote-replacements`."
(let* ((notes my-sidenote-list)
       (pos (length my-sidenote-replacements))
       (label (plist-get (nth pos notes)  :label))
       (text (org-no-properties (org-element-interpret-data (plist-get (nth pos notes)  :content))))
       (html  (my-org-export-org-to-html text))
       (ccell  (cons  _match (or  (my-org-tufte-export-sidenote (or label (format "%d" pos)) html) "[MISSING SIDENOTE]"))))
  (push ccell my-sidenote-replacements))
_match)

(defun my-org-tufte-replace-sidenote-markers (html backend info)
  "Non modifica HTML, ma raccoglie tutte le sostituzioni (match . replacement)
in `my-sidenote-replacements` per successiva applicazione."
  (when (eq backend 'html-tufte)
    (let ((regexp "<sup><a id=\"fnr\\.[^\"]+\"[^>]*>[^<]*</a></sup>"))
      ;; Esegui solo per raccogliere i match; ignora il valore di ritorno
      (ignore
       (replace-regexp-in-string
        regexp
        #'my-org-subst-match
        html t t))
      ;; restituisci html invariato
      (or html ""))))

(defun my-org-tufte-init (html backend info)
  (sit-for 2)
  (or html ""))
  

(defun my-disable-html-footnote-section (_info)
  "Rimuove la sezione delle note a piè di pagina."
  "")

(advice-add 'org-html-footnote-section :override
            #'my-disable-html-footnote-section)


(defun my-org-tufte-preprocess-sidenotes (backend)
  "Cerca sidenote inline e standard nel buffer e le salva in `my-sidenote-map`.
Ogni nota viene sostituita da un marker univoco §N:label§ nel buffer."
  (when (eq backend 'html-tufte)
    (save-excursion
      ;; Inline footnotes: [fn::Testo...]
      (goto-char (point-min))
      (setq my-sidenote-list
            (my-org-footnote-occurrences))
      ;(my-org-print-footnote-occurrences)
      )))

(add-hook
 'org-export-before-processing-hook
 #'my-org-tufte-preprocess-sidenotes)

; (setq org-export-filter-final-output-functions nil)

(add-to-list 'org-export-filter-final-output-functions
    #'my-org-tufte-init)

(add-to-list 'org-export-filter-final-output-functions
    #'my-org-apply-sidenote-replacements)

(add-to-list 'org-export-filter-final-output-functions
    #'my-org-tufte-replace-sidenote-markers)

