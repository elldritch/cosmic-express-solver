FROM haskell:9.4.7-slim-buster AS builder

RUN cabal update

WORKDIR /tmp
COPY . .
RUN cabal build

FROM aibasel/downward:22.06

ENV PATH="/workspace/downward/builds/release/bin:${PATH}" LANG="C.utf8"

COPY --from=builder \
  /tmp/dist-newstyle/build/x86_64-linux/ghc-9.4.7/cosmic-express-solver-0.1.0.0/x/cosmic-solver/build/cosmic-solver/cosmic-solver \
  /usr/local/bin

# For testing, for now. Run `cosmic-solver`.
ENTRYPOINT [ "/usr/local/bin/cosmic-solver", "--no-cache", "--datadir", "whatever", "--constellation", "andromeda", "--level", "1" ]
