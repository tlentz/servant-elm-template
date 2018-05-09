FROM fpco/stack-build:lts-11.0 as build
RUN mkdir /opt/build
COPY server /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM ubuntu:16.04
RUN mkdir -p /opt/servant-elm-template
ARG BINARY_PATH
WORKDIR /opt/servant-elm-template
Run cd /opt && ls

Expose 7000

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq5 libgmp10 ca-certificates netbase && \
    apt-get clean

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-9.0/8.0.2/bin .
COPY client/dist/ client/dist/
COPY db/migrate/ db/migrate/
Run cd /opt/servant-elm-template && ls

CMD ["/opt/servant-elm-template/app"]