FROM ubuntu:22.04

ENV TZ=Europe
ENV DEBIAN_FRONTEND noninteractive

# Install Stack and GHC
RUN apt -y update
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh
ENV PATH="/root/.local/bin:${PATH}"
RUN stack --resolver ghc-9.6.4 setup

RUN stack install omolu

WORKDIR /peter-lang

COPY . /peter-lang

# cancel the build if there are formatting errors
RUN ormolu --mode check $(find . -name '*.hs')
# build project
RUN stack build
RUN stack test
# pre-commit
RUN git init . && pre-commit install-hooks
RUN pre-commit run --all-files
