FROM      khaio/kerl

VERSION r16b03

RUN KERL_CONFIGURE_OPTIONS=--enable-hipe kerl build R16B03 r16b03
RUN kerl install r16b03 /opt/erlang/r16b03
RUN ln -s /opt/erlang/r16b03 /opt/erlang/current
 
RUN cd /usr/local/bin && find /opt/erlang/current/bin -exec ln -s '{}' \;
 
## Install Rebar
RUN cd /opt/erlang && git clone git://github.com/rebar/rebar.git && \
    cd /opt/erlang/rebar && ./bootstrap && \
    ln -s /opt/erlang/rebar/rebar /usr/local/bin/rebar
