FROM ubuntu:22.04

ENV TZ=Europe
ENV DEBIAN_FRONTEND noninteractive

# Install Stack and GHC
RUN apt -y update
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh
ENV PATH="/root/.local/bin:${PATH}"
RUN stack --resolver ghc-9.6.4 setup

WORKDIR /peter-lang

COPY . /peter-lang

RUN stack build 
RUN stack test
