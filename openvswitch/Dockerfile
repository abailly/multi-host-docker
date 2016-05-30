FROM ubuntu:trusty

RUN apt-get update && apt-get install -y build-essential fakeroot debhelper \
                        autoconf automake bzip2 libssl-dev \
                        openssl graphviz python-all procps \
                        python-qt4 python-zopeinterface \
                        python-twisted-conch libtool

RUN apt-get install -y wget

RUN wget -O - http://openvswitch.org/releases/openvswitch-2.3.1.tar.gz | tar xzf -

WORKDIR openvswitch-2.3.1

ENV DEB_BUILD_OPTIONS='parallel=8 nocheck' 
RUN fakeroot debian/rules binary
