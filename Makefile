GIT_COMMIT:=$(shell git rev-parse --short HEAD)

.PHONY: build
build: Dockerfile
	docker build -t alokawi/apollo-workshop-akumar . 

.PHONY: push
push: build
	docker tag alokawi/apollo-workshop-akumar alokawi/apollo-workshop-akumar:$(GIT_COMMIT)
	docker push alokawi/apollo-workshop-akumar:$(GIT_COMMIT) 

.PHONY: deploy
deploy: push
	apollo deploy -e production -m ikman -t $(GIT_COMMIT)
