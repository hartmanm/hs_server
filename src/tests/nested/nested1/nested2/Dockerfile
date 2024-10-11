# Copyright (c) 2023 Michael Neill Hartman. All rights reserved.
# mnh_license@proton.me
# https://github.com/hartmanm

FROM   ubuntu:jammy AS with_haskell
RUN    apt-get update -y
RUN    apt-get install haskell-platform -y
RUN    cabal update

FROM   with_haskell AS with_dependencies
RUN    mkdir -p                     /server/app
COPY   src/server.cabal             /server
COPY   src/initial_main             /server/app/Main.hs
RUN    cd /server;                  cabal install
COPY   src/run_hs_server            /
RUN    chmod 755                    /run_hs_server
COPY   src/Main.hs                  /server/app/Main.hs

FROM   with_dependencies AS with_runtime_config
COPY   generated/modified_routes    /server

ENTRYPOINT ["./run_hs_server"]
