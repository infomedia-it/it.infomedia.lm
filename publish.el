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
    (save-excursion
      ;; Inline footnotes: [fn::Testo...]
      (goto-char (point-min))
      
      (while (re-search-forward "\\[fn::\\(.*?\\)\\]" nil t)
        (let* ((end (match-end 0)) ; salva subito la fine del match
               (text (match-string 1))
               (label (format "%d" my-sidenote-counter))
               (marker (format "§N:%d§" my-sidenote-counter))
               (html (string-trim+ (org-export-string-as text  'html t)))
               (value (format "<label for=\"%s\" class=\"margin-toggle sidenote-number\"></label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/><span class=\"sidenote\">%s</span>" label label html)))
          (puthash marker value my-sidenote-map)
          (message (format "%s ::: %s" marker value)
          (setq my-sidenote-counter (1+ my-sidenote-counter)))))
      ;; Fai lo stesso per le note standard
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "\\[fn:\\([a-zA-Z0-9]]+\\): *\\(.*\\)\\]" nil t)
          (let* ((label (match-string 1))
                 (text (match-string 2))
                 (marker (format "§N:%s§" label))
                 (html (string-trim+ (org-export-string-as text 'html t)))
                 (value (format "<label for=\"%s\" class\"margin-toggle sidenote-number\"></label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/><span class=\"sidenote\">%s</span>" label label html)))
            (puthash marker value my-sidenote-map)
            (setq my-sidenote-counter (1+ my-sidenote-counter))
            (message (format "%s ::: %s" marker value)))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\[fn:\\([^:]]+\\)\\] *\\(.*\\)$" nil t)
        (let* ((label (match-string 1))
               (text (match-string 2))
               (marker (format "§N:%s§" label))
               (html (string-trim+ (org-export-string-as text 'html t)))
               (value (format "<label for=\"%s\" class\"margin-toggle sidenote-number\"></label><input type=\"checkbox\" id=\"%s\" class=\"margin-toggle\"/><span class=\"sidenote\">%s</span>" label label html)))
          (puthash marker value my-sidenote-map)
          (message (format "%s ::: %s" marker value)))
        (setq my-sidenote-counter (1+ my-sidenote-counter))
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


(defun org-html-footnote-section (_info) "")


(defun my-org-tufte-preprocess-footnote-reference (html backend info)
  "Sostituisce tutti i marker §N:label§ con sidenote HTML Tufte."
  (when (eq backend 'html)
    (replace-regexp-in-string
     ;; Regex che trova footnote HTML standard con id "fnr.LABEL"
     "<sup><a id=\"fnr\\.\\([^\"]+\\)\"[^>]*>[^<]*</a></sup>"
     (lambda (match)
       (let* ((label (match-string 1 match))
              (id (format "§N:%s§" label))
              (text (gethash id my-sidenote-map)))org-export-filter-footnote-reference-functions
         (if text text id))) ;; fallback se label non trovata
     html)))

(add-to-list 'org-export-filter-footnote-definition-functions
             (lambda (s backend info) ""))

(add-to-list 'org-export-filter-footnote-definition-functions #'my-org-tufte-preprocess-footnote-reference)

(describe-variable 'org-export-filter-footnote-definition-functions)

;(add-hook 'org-export-before-processing-hook #'my-org-tufte-preprocess-sidenotes)

;(add-to-list 'org-export-filter-final-output-functions #'my-org-tufte-replace-sidenote-markers)

;(setq org-export-filter-final-output-functions nil)

(defun my-org-empty-hooks ()
  (interactive)
  (setq org-export-before-parsing-hook nil)
  (setq org-export-before-processing-hook nil)
  (setq org-export-after-processing-hook nil)
  (setq org-export-before-processing-functions nil)
  (setq org-export-filter-final-output-functions nil)
  (setq org-export-filter-footnote-reference-functions nil)
  (setq org-export-filter-paragraph-functions nil)
  (setq org-export-filter-section-functions nil)
  (setq org-export-filter-headline-functions nil)
  (setq org-export-filter-plain-list-functions nil)
  (setq org-export-filter-item-functions nil)
  (setq org-export-filter-link-functions nil))



(defvar-local my-sidenote-counter 1)

(defun my-org-tufte-sidenote (contents info)
  "Custom exporter for inline footnotes to Tufte sidenotes."
  (let ((n my-sidenote-counter))
    (setq my-sidenote-counter (1+ n))
    (format "<label for=\"sn-%d\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"sn-%d\" class=\"margin-toggle\">
<span class=\"sidenote\">%s</span>" n n contents)))

(advice-add 'org-html-footnote-reference :override
            (lambda (footnote info)
              (if (stringp footnote)
                  ;; inline footnote: [fn::...]
                  (my-org-tufte-sidenote footnote info)
                ;; regular reference: delegate to original function
                (org-html-format-footnote-reference footnote info))))




(defvar my-sidenote-counter 1
  "Contatore globale per le sidenote.")

(defun my-org-tufte-sidenote-content (footnote info)
  "Esporta il contenuto della sidenote da un footnote OrgMode."
  (cond
   ;; Footnote inline: [fn:: ...]
   ((stringp footnote)
    (org-export-string-as footnote 'html t))
   ;; Footnote [fn:label: ...] => cdr è stringa
   ((and (listp footnote)
         (stringp (cdr footnote)))
    (org-export-string-as (cdr footnote) 'html t))
   ;; Footnote [fn:label] => cdr è AST
   ((and (listp footnote)
         (listp (cdr footnote)))
    (org-export-data (cdr footnote) info))
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


(setq org-export-show-temporary-export-buffer t)
(setq debug-on-error t)


(defun my-org-print-footnotes ()
  "Parse current Org buffer and print all footnote definitions."
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (footnotes
          (org-element-map ast 'footnote-definition
            (lambda (fn)
              (let ((label (org-element-property :label fn))
                    (contents (org-element-interpret-data (org-element-contents fn))))
                (format "[fn:%s] %s" label contents))))))
    (message "Footnotes:\n%s" (string-join footnotes "\n\n"))))


(defun my-org-show-footnotes-in-buffer ()
  "Show all footnote definitions in a dedicated buffer."
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (footnotes
          (org-element-map ast 'footnote-definition
            (lambda (fn)
              (let ((label (org-element-property :label fn))
                    (contents (org-element-interpret-data (org-element-contents fn))))
                (format "[fn:%s] %s" label contents))))))
    (with-output-to-temp-buffer "*Footnotes*"
      (princ (string-join footnotes "\n\n")))))

(defun my-org-collect-all-footnotes ()
  "Raccoglie e stampa tutte le note a piè di pagina del buffer corrente,
incluse sia quelle definite separatamente sia quelle anonime inline."
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (footnotes '())
         ;; raccogliamo le definizioni etichettate
         (defs
          (org-element-map ast 'footnote-definition
            (lambda (fn)
              (let ((label (org-element-property :label fn))
                    (contents (org-element-interpret-data (org-element-contents fn))))
                (push (format "[fn:%s] %s" label contents) footnotes)))))
         ;; raccogliamo le inline e referenze dirette
         (refs
          (org-element-map ast 'footnote-reference
            (lambda (ref)
              (let ((label (org-element-property :label ref))
                    (inline (org-element-contents ref)))
                (cond
                 ;; caso inline: [fn:: ...]
                 ((null label)
                  (push (format "[fn::] %s"
                                (org-element-interpret-data inline))
                        footnotes))
                 ;; caso etichettato ma definito inline (raro)
                 ((and label inline)
                  (push (format "[fn:%s] %s"
                                label
                                (org-element-interpret-data inline))
                        footnotes))
                 ;; altrimenti è solo un riferimento: non lo stampiamo
                 ))))))
    ;; stampa risultato
    (with-output-to-temp-buffer "*All Footnotes*"
      (princ (string-join (reverse footnotes) "\n\n")))))


(defun my-org-export-with-footnote-log ()
  "Log footnotes before exporting to HTML."
  (interactive) 
  (let* ((ast (org-element-parse-buffer))
         (footnotes-references
          (org-element-map ast 'footnote-reference
            (lambda (fn)
              (let ((label (org-element-property :label fn))
                    (contents (org-element-interpret-data (org-element-contents fn))))
                (format "[fn:%s] %s" label contents))))))
         (footnotes-definitions
          (org-element-map ast 'footnote-definition
            (lambda (fn)
              (let ((label (org-element-property :label fn))
                    (contents (org-element-interpret-data (org-element-contents fn))))
                (format "[fn:%s] %s" label contents))))))
    (message "FOOTNOTES:\n%s" (string-join footnotes "\n\n"))
    ;; continua con l’esportazione standard
    (org-export-as 'html)))



(defun my-org-footnote-list ()
  "Restituisce una lista strutturata di tutte le note nel buffer Org corrente.
Ogni elemento è un alist con le chiavi:
  :label   → etichetta della nota (stringa o nil per inline),
  :index   → numero d’ordine (1-based),
  :content → contenuto testuale della nota."
  (interactive)
  (let* ((ast (org-element-parse-buffer))
         (footnotes '())
         (counter 0))
    ;; definizioni esplicite
    (org-element-map ast 'footnote-definition
      (lambda (fn)
        (setq counter (1+ counter))
        (push `(:label ,(org-element-property :label fn)
                :index ,counter
                :content ,(org-element-interpret-data (org-element-contents fn)))
              footnotes)))
    ;; riferimenti inline (anonimi o con contenuto diretto)
    (org-element-map ast 'footnote-reference
      (lambda (ref)
        (let ((label (org-element-property :label ref))
              (inline (org-element-contents ref)))
          (when inline
            (setq counter (1+ counter))
            (push `(:label ,label
                    :index ,counter
                    :content ,(org-element-interpret-data inline))
                  footnotes)))))
    (reverse footnotes)))


(defun my-org-print-footnote-list ()
  "Visualizza la lista strutturata delle note del buffer corrente Org."
  (interactive)
  (let ((notes (my-org-footnote-list)))
    (with-output-to-temp-buffer "*Footnotes*"
      (dolist (n notes)
        (princ (format "[%d] %s%s\n\n"
                       (plist-get n :index)
                       (if (plist-get n :label)
                           (format "[fn:%s] " (plist-get n :label))
                         "[fn::] ")
                       (plist-get n :content)))))))



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

(defvar my-sidenote-list nil)

(defun string-trim+ (s)
  "Rimuove spazi bianchi e tag <p>...</p> attorno a S, se presenti."
  (let* ((trimmed (string-trim s))
         (stripped
          (if (string-match "<p>\n*\\(.*?\\)</p>[\n\t]*" trimmed)
              (match-string 1 trimmed)
            trimmed)))
    (string-trim stripped)))

(defun my-org-tufte-preprocess-sidenotes (backend)
  "Cerca sidenote inline e standard nel buffer e le salva in `my-sidenote-map`.
Ogni nota viene sostituita da un marker univoco §N:label§ nel buffer."
  (when (eq backend 'html)
    (save-excursion
      ;; Inline footnotes: [fn::Testo...]
      (goto-char (point-min))
      (setq my-sidenote-list
            (my-org-footnote-occurrences))
      ;(my-org-print-footnote-occurrences)
      )))

(add-hook 'org-Export-Before-Processing-Hook
   #'my-org-tufte-preprocess-sidenotes)

(defun my-org-tufte-export-sidenote (id content)
    (format "<label for=\"sn-%s\" class=\"margin-toggle sidenote-number\"></label>
<input type=\"checkbox\" id=\"sn-%s\" class=\"margin-toggle\">
<span class=\"sidenote\">%s</span>" id id content))

(defun my-org-export-org-to-html (org-text)
  "Converte una stringa ORG in HTML, disabilitando i filtri globali temporaneamente."
  (string-trim
   (let ((org-export-filter-final-output-functions nil)
         (org-export-before-processing-hook nil) 
         (org-export-filter-plain-text-functions nil)
         (org-export-filter-paragraph-functions nil))
     (string-trim+ (org-export-string-as org-text 'html t '(:with-footnotes nil))))))

(defun my-org-tufte-replace-sidenote-markers (html backend info)
  "Sostituisce in HTML tutte le note <sup><a ...> con elementi Tufte-style dalla lista `my-sidenote-list`.
Le note sono abbinate in ordine di apparizione."
  (when (eq backend 'html)
    (let ((notes my-sidenote-list)  ;; lista ordinata dei replacement
          (pos 0))
      (replace-regexp-in-string
       ;; trova i riferimenti ai footnote, es: <sup><a id="fnr.1" href="#fn.1">1</a></sup>
       "<sup><a id=\"fnr\\.\\([^\"]+\\)\"[^>]*>[^<]*</a></sup>"
       (lambda (_match)
         (let* ((label (plist-get (nth pos notes)  :label))
                (text (org-element-interpret-data (plist-get (nth pos notes)  :content)))
                (html  (my-org-export-org-to-html text))
                )
           (setq pos (1+ pos))
           (or  (my-org-tufte-export-sidenote (or label (format "%d" pos)) text) "[MISSING SIDENOTE]")))  ;; fallback difensivo
       html)))))

(add-to-list 'org-export-filter-final-output-functions
    #'my-org-tufte-replace-sidenote-markers)


(defun my-disable-html-footnote-section (_info)
  "Rimuove la sezione delle note a piè di pagina."
  "")

(advice-add 'org-html-footnote-section :override #'my-disable-html-footnote-section)


(my-org-export-org-to-html
 "Una nota con link [[https://example.com][qui]] e un riferimento a nota.")
