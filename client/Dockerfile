############################################################
###Step 1
############################################################
FROM node:9.11

RUN apt-get update

WORKDIR /var/app/client

ENTRYPOINT cd /etc && echo "$(tail -n +2 hosts)" > hosts2 && echo -e $'\r\n0.0.0.0 localhost' >> hosts2 && echo "$(cat hosts2)" > hosts && rm -f hosts2 && cd /var/app && /bin/bash
