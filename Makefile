all: build
.PHONY: all

build: backend frontend
	mkdir -p output/static/

	cp -rf output/backend/bin output/
	chmod -R 777 output/bin
	cp -rf output/frontend/bin/frontend-exe.jsexe/* output/static/
	cp -rf frontend/static/* output/static/
	chmod -R 777 output/static
	rm output/backend
	rm output/frontend
.PHONY: build

backend:
	nix-build -o output/backend -A ghc.croe-backend
.PHONY: backend

frontend:
	nix-build -o output/frontend -A ghcjs.croe-frontend
.PHONY: frontend

clean:
	rm -rf output

deploy:
	./local_scripts/deploy.sh

count:
	tokei --exclude reflex-platform/
