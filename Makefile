REBAR := $(shell which rebar3 2>/dev/null || which ./rebar3)
SUBMODULES = build_utils
SUBTARGETS = $(patsubst %,%/.git,$(SUBMODULES))

UTILS_PATH := build_utils
TEMPLATES_PATH := .

# Name of the service
SERVICE_NAME := bender
# Service image default tag
SERVICE_IMAGE_TAG ?= $(shell git rev-parse HEAD)
# The tag for service image to be pushed with
SERVICE_IMAGE_PUSH_TAG ?= $(SERVICE_IMAGE_TAG)

# Base image for the service
BASE_IMAGE_NAME := service-erlang
BASE_IMAGE_TAG := da0ab769f01b650b389d18fc85e7418e727cbe96

# Build image tag to be used
BUILD_IMAGE_TAG := 442c2c274c1d8e484e5213089906a4271641d95e

CALL_ANYWHERE := all submodules rebar-update compile xref lint dialyze start devrel release clean distclean check

CALL_W_CONTAINER := $(CALL_ANYWHERE) test

all: compile

-include $(UTILS_PATH)/make_lib/utils_container.mk
-include $(UTILS_PATH)/make_lib/utils_image.mk

.PHONY: $(CALL_W_CONTAINER)

# CALL_ANYWHERE
$(SUBTARGETS): %/.git: %
	git submodule update --init $<
	touch $@

submodules: $(SUBTARGETS)

rebar-update:
	$(REBAR) update

compile: submodules rebar-update
	$(REBAR) compile

xref: submodules
	$(REBAR) xref

lint:
	elvis rock

dialyze:
	$(REBAR) dialyzer

check: xref lint dialyze

start: submodules
	$(REBAR) run

devrel: submodules
	$(REBAR) release

release: distclean
	$(REBAR) as prod release

clean:
	$(REBAR) clean

distclean:
	$(REBAR) clean
	rm -rf _build

# CALL_W_CONTAINER
test: submodules
	$(REBAR) ct
