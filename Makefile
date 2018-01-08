IMAGE := sure-deploy

ifeq ($(TRAVIS_BRANCH),master)
	CONTAINER_NAME = $(IMAGE):$(TRAVIS_BUILD_NUMBER)
else
	ifeq ($(TRAVIS_BUILD_NUMBER),)
		CONTAINER_NAME = $(IMAGE)
	else
		CONTAINER_NAME = $(IMAGE)-$(TRAVIS_BRANCH_NAME):$(BUILD_NUMBER)
	endif
endif

.PHONY: all
all:
	jbuilder build

.PHONY: clean
clean:
	jbuilder clean

.PHONY: docker-image
docker-image:
	@echo 'Building docker image $(CONTAINER_NAME)'
	docker build -f Dockerfile -t $(CONTAINER_NAME) .

.PHONY: docker-push
docker-push: QUALIFIED_CONTAINER_NAME = leonidasfromxiv/$(CONTAINER_NAME)
docker-push:
	@echo "Pushing docker image '$(CONTAINER_NAME)' to docker hub"
	docker login -u "$(DOCKER_USERNAME)" -p "$(DOCKER_PASSWORD)"
	docker tag $(CONTAINER_NAME) $(QUALIFIED_CONTAINER_NAME)
	docker push $(QUALIFIED_CONTAINER_NAME)
