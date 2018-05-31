############################################################
###Step 1
############################################################
FROM fpco/stack-build:lts-11.0 as build


RUN stack setup
RUN stack install hpack
RUN stack install intero


COPY . /var/app/server

RUN cd /var/app/server && stack build --system-ghc

ENTRYPOINT cd /etc && echo "$(tail -n +2 hosts)" > hosts2 && echo -e $'\r\n0.0.0.0 localhost' >> hosts2 && echo "$(cat hosts2)" > hosts && rm -f hosts2 && cd /var/app && /bin/bash
WORKDIR /var/app/server
