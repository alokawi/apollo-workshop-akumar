GIT_COMMIT:=$(shell git rev-parse --short HEAD)

.PHONY: build
build: Dockerfile
	docker build -t alokawi/apollo-workshop-akumar . 

.PHONY: push
push: build
	docker tag alokawi/apollo-workshop-akumar alokawi/apollo-workshop-akumar:$(GIT_COMMIT)
	docker push alokawi/apollo-workshop-akumar:$(GIT_COMMIT) 

.PHONY: test
test:build
	bin/apollo validate

.PHONY: deploy
deploy: push
	bin/apollo deploy -e production -m bikroy -t $(GIT_COMMIT) --bootstrap
