FROM alpine:3.6

RUN apk add --no-cache --update curl zip bash

WORKDIR /root

COPY .stack-work/install/x86_64-linux/lts-7.2/8.0.1/bin/rea-backend rea-backend

CMD ["rea-backend"]
