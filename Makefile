all: build
.PHONY: all

build: backend frontend
.PHONY: build

backend:
	mkdir -p output/bin
	# build and copy server binary
	stack build croe-backend
	cp -rf `stack path --local-install-root`/bin/croe-backend-exe output/bin/croe
	# copy config dir
	cp -rf backend/config output/
.PHONY: backend

frontend:
	mkdir -p output/static/
	mkdir -p output/temp/
	# build and copy GHCJS generated files
	nix-build -o output/temp/frontend -A ghcjs.croe-frontend
	cp -rf output/temp/frontend/bin/croe-frontend-exe.jsexe/* output/static/
	# build CSS and copy other static files
	frontend/sass/build.sh
	cp -rf frontend/static/* output/static/
.PHONY: frontend

clean:
	rm -rf output

deploy:
	./local_scripts/deploy.sh

count:
	tokei --exclude reflex-platform --exclude frontend/sass/bulma --exclude frontend/static
