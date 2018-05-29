############################################################
###Step 1
############################################################
FROM fpco/stack-build:lts-11.0 as build
# FROM haskell:latest as build

RUN apt-get update

RUN npm cache clean -f
RUN npm install -g n
RUN n 9
RUN npm install -g npm
RUN npm install -g elm@0.18.0 -unsafe-perm=true --allow-root
RUN npm install -g elm-test@0.18.0
RUN npm install -g typescript
RUN npm install -g --save-dev webpack
RUN npm install -g webpack-dev-server
RUN npm install -g -D webpack-cli

RUN stack setup
RUN stack install hpack
RUN stack install intero


COPY . /var/app

RUN cd /var/app/server && stack install --local-bin-path bin
RUN cd /var/app/server && stack build --system-ghc
RUN cd /var/app/server && stack exec code-generator
RUN cd /var/app/client && npm rebuild node-sass --force` && npm install && elm package install --yes && elm make

ENTRYPOINT cd /etc && echo "$(tail -n +2 hosts)" > hosts2 && echo -e $'\r\n0.0.0.0 localhost' >> hosts2 && echo "$(cat hosts2)" > hosts && rm -f hosts2 && cd /var/app && /bin/bash
WORKDIR /var/app