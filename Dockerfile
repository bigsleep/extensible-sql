FROM tkaaad97/haskell-docker:8.2.2

# PATH config
RUN printf "export PATH=$PATH" > ~/.bash_profile

# install dev tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        default-libmysqlclient-dev \
        libpcre3-dev \
        libpq-dev \
        make \
        pkgconf \
        xz-utils

WORKDIR /app/

ENTRYPOINT []
CMD ["bash"]
