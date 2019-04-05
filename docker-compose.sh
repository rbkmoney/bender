#!/bin/bash
cat <<EOF
version: '2.1'
services:

  ${SERVICE_NAME}:
    image: ${BUILD_IMAGE}
    volumes:
      - .:$PWD
      - $HOME/.cache:/home/$UNAME/.cache
    working_dir: $PWD
    command: /sbin/init
    depends_on:
      machinegun:
        condition: service_healthy

  machinegun:
    image: dr2.rbkmoney.com/rbkmoney/machinegun:f89b294ac584d212264be60cc4db242aeaecce89
    command: /opt/machinegun/bin/machinegun foreground
    volumes:
      - ./test/machinegun/config.yaml:/opt/machinegun/etc/config.yaml
    healthcheck:
      test: "curl http://localhost:8022/"
      interval: 5s
      timeout: 1s
      retries: 12

EOF

