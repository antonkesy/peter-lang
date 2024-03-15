FROM ubuntu:22.04

ENV TZ=Europe
ENV DEBIAN_FRONTEND noninteractive

# Install Stack and GHC
RUN apt -y update
RUN apt-get -y install curl
RUN curl -sSL https://get.haskellstack.org/ | sh
ENV PATH="/root/.local/bin:${PATH}"
RUN stack --compiler ghc-9.4.7 setup

# pre-commit
RUN apt-get -y install python3-pip
RUN pip install pre-commit

# formatter
RUN stack install ormolu

WORKDIR /peter-lang

COPY . /peter-lang

# cancel the build if there are formatting errors
RUN ormolu --mode check $(find . -name '*.hs')

# build project
RUN stack build

# run tests
RUN stack test

WORKDIR /peter-lang/test/E2E/Interpreter/examples
RUN ./check_examples.sh
WORKDIR /peter-lang

# pre-commit
RUN git init . && pre-commit install-hooks
RUN pre-commit run --all-files
