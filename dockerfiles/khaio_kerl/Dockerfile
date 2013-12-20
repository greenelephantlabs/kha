FROM      khaio/ubuntu:13.10
 
RUN apt-get update
RUN apt-get install -y git build-essential libncurses5-dev openssl libssl-dev curl
# For HiPE, requires M4
RUN apt-get install -y m4
 
RUN mkdir -p /opt/erlang/ && \
    curl -O https://raw.github.com/spawngrid/kerl/master/kerl && chmod a+x kerl && \
    mv kerl /opt/erlang/ && \
    ln -s /opt/erlang/kerl /usr/local/bin/kerl

RUN kerl update releases
