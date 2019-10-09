ARG HASKELL_VERSION
ARG LEXML_ALPINE_GLIBC_VERSION
FROM haskell:$HASKELL_VERSION as build-base
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
WORKDIR /root
RUN cabal user-config init
COPY packages /root/.cabal/packages/
RUN export LC_ALL=en_US.UTF-8 && \
    if [ ! -e "/root/.cabal/packages/hackage.haskell.org" ] ; then \
      cabal v2-update --verbose=3 ; \
    fi
RUN LC_ALL=en_US.UTF-8 cabal v2-install alex happy
COPY lexml-parser.cabal /root/lexml-parser.cabal
COPY src /root/src/
COPY LICENSE /root/
ARG ncpus=1
RUN cabal v2-build all 
RUN find /root/dist-newstyle/build -type f -executable -exec cp '{}' /root ';'
FROM lexmlbr/alpine-glibc:$LEXML_ALPINE_GLIBC_VERSION 
WORKDIR /root
RUN apk add --no-cache gmp
COPY --from=build-base /root/linkertool /root/simplelinker /root/LinkerServer /usr/bin/
COPY version /etc/lexml-linker-build-version
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
CMD ["/usr/bin/linkertool"]
