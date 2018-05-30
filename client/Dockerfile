############################################################
###Step 1
############################################################
FROM node:9.11

RUN apt-get update

# 
# 
# RUN npm install -g typescript
# # RUN npm install -g --save-dev webpack
# # RUN npm install -g webpack-dev-server

# # RUN npm install -g -D webpack-cli

COPY . /var/app

WORKDIR /var/app/client
RUN npm install
RUN npm install -g elm@0.18.0 -unsafe-perm=true --allow-root
RUN npm install -g elm-test@0.18.0
RUN elm package install --yes 
RUN elm make

ENTRYPOINT cd /etc && echo "$(tail -n +2 hosts)" > hosts2 && echo -e $'\r\n0.0.0.0 localhost' >> hosts2 && echo "$(cat hosts2)" > hosts && rm -f hosts2 && cd /var/app && /bin/bash
