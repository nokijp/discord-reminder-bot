FROM ubuntu:18.04 AS builder
RUN apt update && \
    DEBIAN_FRONTEND=noninteractive apt install -y wget && \
    wget -qO- https://get.haskellstack.org/ | sh
WORKDIR /tmp
RUN stack setup --resolver=lts-14.10
COPY stack.yaml .
COPY package.yaml .
RUN stack build --dry-run
WORKDIR /reminder-bot
COPY . .
RUN stack install

FROM ubuntu:18.04
RUN apt update && \
    DEBIAN_FRONTEND=noninteractive apt install -y ca-certificates netbase tzdata && \
    ln -sf /usr/share/zoneinfo/Asia/Tokyo /etc/localtime && \
    rm -rf /var/lib/apt/lists/*
COPY --from=builder /root/.local/bin/reminder-bot .
CMD /reminder-bot
