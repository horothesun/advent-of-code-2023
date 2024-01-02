# The only purpose of this file is to get Renovate updates on the Temurin JDK version.
# The CI uses the following JAVA_VERSION value to configure its JDK setup (through `scripts/get_java_version.sh`).

ARG JAVA_VERSION=17

FROM eclipse-temurin:${JAVA_VERSION}-jdk-jammy AS builder

FROM eclipse-temurin:${JAVA_VERSION}-jdk-jammy
