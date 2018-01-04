IMAGE := sure-deploy

ifeq ($(BRANCH_NAME),master)
	CONTAINER_NAME = $(IMAGE):$(BUILD_NUMBER)
else
	ifeq ($(BUILD_NUMBER),)
		CONTAINER_NAME = $(IMAGE)
	else
		CONTAINER_NAME = $(IMAGE)-$(BRANCH_NAME):$(BUILD_NUMBER)
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
docker-push:
	@echo "Pushing docker image to registry"
	docker tag $(CONTAINER_NAME) docker-registry.issuu.com/$(CONTAINER_NAME)
	docker push docker-registry.issuu.com/$(CONTAINER_NAME)
