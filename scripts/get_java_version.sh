#!/bin/bash

JAVA_VERSION=$(grep "ARG JAVA_VERSION=" "Dockerfile" | grep -o "[^=]*$")

[[ -z "${JAVA_VERSION}" ]] && echo "Error: JAVA_VERSION not found in Dockerfile" && exit 123

export JAVA_VERSION
