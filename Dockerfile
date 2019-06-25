FROM haskell:8.4 as build-base
WORKDIR /root
RUN cabal update --verbose=3 && \
    cabal install --verbose=3 alex happy
COPY ./lexml-parser.cabal /root/lexml-parser.cabal
ARG ncpus=1
RUN cabal install --verbose=3 --only-dependencies -j${ncpus}
COPY . /root
ARG ncpus=1
RUN cabal install --verbose=3 -j${ncpus}

FROM lexmlbr/alpine-glibc:3.10
WORKDIR /root
RUN apk add --no-cache gmp
COPY --from=build-base /root/.cabal/bin/linkertool /root/.cabal/bin/simplelinker /root/.cabal/bin/LinkerServer /usr/bin/
CMD ["/usr/bin/linkertool"]
