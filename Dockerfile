FROM haskell:9.6 AS build

WORKDIR /build

RUN apt-get update && apt-get install -y libsqlite3-dev && rm -rf /var/lib/apt/lists/*

COPY inf221-project-main/project.cabal .
RUN cabal update && cabal build --only-dependencies

COPY inf221-project-main/ .
RUN cabal build && \
    cp $(cabal list-bin project) /build/server

FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y libsqlite3-0 libgmp10 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /build/server /app/server

EXPOSE 8080

CMD ["/app/server"]
