#!/bin/bash

. version

ncpus_default=`cat /proc/cpuinfo  | grep processor | tail -n 1 | sed -e 's/processor.*: *//g'`
ncpus_default=$(( $ncpus_default + 1))

ncpus="${ncpus:-${ncpus_default}}"


function getExtraParameters {
  if [ ! -z "$http_proxy" ]; then
    PROXY_BASE=$(echo $http_proxy | cut -d/ -f3)
    PROXY_HOST=$(ip addr list docker0 | grep "inet " | cut -d' ' -f6 | cut -d/ -f1)
    PROXY_PORT=$(echo $PROXY_BASE | cut -d: -f2) 
    PROXY="http://"$(ip addr list docker0 |grep "inet " |cut -d' ' -f6|cut -d/ -f1)":3128"
    echo "--build-arg http_proxy=$PROXY --build-arg https_proxy=$PROXY --build-arg http_host=$PROXY_HOST \
          --build-arg http_port=$PROXY_PORT --build-arg HTTP_PROXY=$PROXY"
  else
    echo ""
  fi
}

EXTRA_PARAMS=$(getExtraParameters)

docker build ${EXTRA_PARAMS} --build-arg ncpus=$ncpus --build-arg version=${LINKER_VERSION} --build-arg HASKELL_VERSION=$HASKELL_VERSION \
       --build-arg LEXML_ALPINE_GLIBC_VERSION=$LEXML_ALPINE_GLIBC_VERSION \
       . -t lexmlbr/lexml-linker:${LINKER_VERSION}
