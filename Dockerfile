FROM haskell:8.8.4-buster AS builder

# initialize stack
RUN stack install --resolver=lts-16.20 base

# install dependencies
WORKDIR /tmp
COPY stack.yaml .
COPY package.yaml .
RUN stack build --only-dependencies --system-ghc

# build
WORKDIR /reminder-bot
COPY . .
RUN stack build --copy-bins --system-ghc


FROM debian:buster-slim
RUN apt-get update && apt-get install -y netbase ca-certificates \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
COPY --from=builder /root/.local/bin/reminder-bot .
CMD /reminder-bot
