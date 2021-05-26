FROM docker.io/zhonghl003/minideb:buster

ENV HOME="/" \
    OS_ARCH="amd64" \
    OS_FLAVOUR="debian-10" \
    OS_NAME="linux"\
    TZ=Asia/Shanghai
COPY prebuildfs /
COPY postgresql-repmgr-11.11.0-15-linux-amd64-debian-10/files /opt/qingcloud
COPY gosu-1.12.0-2-linux-amd64-debian-10 /opt/qingcloud
# Install required system packages and dependencies
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN echo "deb http://mirrors.aliyun.com/debian/ buster main" > /etc/apt/sources.list
RUN install_packages acl ca-certificates curl gzip libbsd0 libc6 libedit2 libffi6 libgcc1 libgmp10 libgnutls30 libhogweed4 libicu63 libidn2-0 libldap-2.4-2 liblzma5 libnettle6 libp11-kit0 libreadline7 libsasl2-2 libsqlite3-0 libssl1.1 libstdc++6 libtasn1-6 libtinfo6 libunistring2 libuuid1 libxml2 libxslt1.1 locales procps tar zlib1g
RUN chmod g+rwX /opt/qingcloud
RUN localedef -c -f UTF-8 -i en_US en_US.UTF-8
RUN update-locale LANG=C.UTF-8 LC_MESSAGES=POSIX && \
    DEBIAN_FRONTEND=noninteractive dpkg-reconfigure locales
RUN echo 'en_GB.UTF-8 UTF-8' >> /etc/locale.gen && locale-gen
RUN echo 'en_US.UTF-8 UTF-8' >> /etc/locale.gen && locale-gen

COPY rootfs /
RUN /opt/qingcloud/scripts/postgresql-repmgr/postunpack.sh
RUN /opt/qingcloud/scripts/locales/add-extra-locales.sh
ENV LANG="en_US.UTF-8" \
    LANGUAGE="en_US:en" \
    NSS_WRAPPER_LIB="/opt/qingcloud/common/lib/libnss_wrapper.so" \
    PATH="/opt/qingcloud/postgresql/bin:/opt/qingcloud/repmgr/bin:/opt/qingcloud/common/bin:$PATH" \
    LD_LIBRARY_PATH="/opt/qingcloud/postgresql/lib"

EXPOSE 5432

USER 1001
ENTRYPOINT [ "/opt/qingcloud/scripts/postgresql-repmgr/entrypoint.sh" ]
CMD [ "/opt/qingcloud/scripts/postgresql-repmgr/run.sh" ]
