# Makefile per it.infomedia.lm – pubblicazione GitHub Pages

# Directory sorgente e destinazione
HTML_DIR = html
CSS_DIR  = css
DEST_DIR = docs
CNAME = $(DEST_DIR)/CNAME
CNAME_BACKUP = $(DEST_DIR)/.CNAME.tmp

clean:
	@if [ -f $(CNAME) ]; then cp $(CNAME) $(CNAME_BACKUP); fi
	rm -rf $(DEST_DIR)/*
	mkdir -p $(DEST_DIR)
	@if [ -f $(CNAME_BACKUP) ]; then mv $(CNAME_BACKUP) $(CNAME); fi


# Obiettivo principale: copia tutto in docs/
publish: clean prepare copy_html copy_css

# Pulisce la cartella docs/
clean:
	@if [ -f $(CNAME) ]; then cp $(CNAME) $(CNAME_BACKUP); fi
	rm -rf $(DEST_DIR)/*
	mkdir -p $(DEST_DIR)
	@if [ -f $(CNAME_BACKUP) ]; then mv $(CNAME_BACKUP) $(CNAME); fi

# Crea le sottocartelle necessarie
prepare:
	mkdir -p $(DEST_DIR)/css

# Copia gli HTML generati da Emacs
copy_html:
	cp -r $(HTML_DIR)/*.html $(DEST_DIR)/

# Copia il CSS Tufte
copy_css:
	cp -r $(CSS_DIR)/* $(DEST_DIR)/css/

# Per visualizzare i file copiati
status:
	@echo "Contenuto in $(DEST_DIR):"
	@find $(DEST_DIR)

# Obiettivo ausiliario per testare localmente con Python
serve:
	cd $(DEST_DIR) && python3 -m http.server 8080

.PHONY: publish clean prepare copy_html copy_css status serve
