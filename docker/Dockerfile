FROM zelinf/haskell-web-fullstack:0.1
ARG http_proxy
ARG https_proxy
COPY ./ /opt/croe/
WORKDIR /opt/croe/
RUN apt-get update \
  && apt-get install -y zlib1g-dev \
  && apt-get install -y libmysqlclient-dev \
  && apt-get install -y libgmp-dev \
  && apt-get install -y libtinfo-dev \
  && apt-get install -y libpcre3-dev&& apt-get install -y libpcre3-dev&& apt-get install -y libpcre3-dev \
  && apt-get install -y dos2unix
RUN . /root/.nix-profile/etc/profile.d/nix.sh \
  && find . -type f -print0 | xargs -0 dos2unix \
  && make frontend
RUN stack build --test --only-dependencies
