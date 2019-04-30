IMAGE := sure-deploy

ifeq ($(TRAVIS_BRANCH),master)
	CONTAINER_NAME = $(IMAGE):$(TRAVIS_BUILD_NUMBER)
else
	ifeq ($(TRAVIS_BUILD_NUMBER),)
		CONTAINER_NAME = $(IMAGE)
	else
		CONTAINER_NAME = $(IMAGE)-$(TRAVIS_BRANCH):$(TRAVIS_BUILD_NUMBER)
	endif
endif

.PHONY: all
all:
	dune build

.PHONY: clean
clean:
	dune clean

.PHONY: docker-image
docker-image:
	@echo 'Building docker image $(CONTAINER_NAME)'
	docker build -f Dockerfile -t $(CONTAINER_NAME) .

.PHONY: docker-push
docker-push: QUALIFIED_CONTAINER_NAME = issuu/$(CONTAINER_NAME)
docker-push:
	@echo "Pushing docker image '$(CONTAINER_NAME)' to docker hub"
	docker login -u "$(DOCKER_USERNAME)" -p "$(DOCKER_PASSWORD)"
	docker tag $(CONTAINER_NAME) $(QUALIFIED_CONTAINER_NAME)
	docker push $(QUALIFIED_CONTAINER_NAME)

.PHONY: format
format:
	dune build @fmt --auto-promote @install
