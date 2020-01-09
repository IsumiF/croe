all: build
.PHONY: all

build: backend frontend
	mkdir -p output/static/

	cp -rf output/backend/bin output/
	cp -rf output/backend/config output/
	chmod -R 777 output/bin
	cp -rf output/frontend/bin/frontend-exe.jsexe/* output/static/
	cp -rf frontend/static/* output/static/
	chmod -R 777 output/static
	rm -rf output/backend
	rm -rf output/frontend
.PHONY: build

backend:
	mkdir -p output/backend/bin
	stack build croe-backend
	cp -rf `stack path --local-install-root`/bin/backend-exe output/backend/bin/croe
	cp -rf backend/config output/backend/
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
