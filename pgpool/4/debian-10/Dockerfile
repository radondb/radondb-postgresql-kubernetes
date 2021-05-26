FROM docker.io/zhonghl003/minideb:buster

ENV HOME="/" \
    OS_ARCH="amd64" \
    OS_FLAVOUR="debian-10" \
    OS_NAME="linux"\
    TZ=Asia/Shanghai

COPY prebuildfs /
# Install required system packages and dependencies
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
RUN echo "deb http://mirrors.aliyun.com/debian/ buster main" > /etc/apt/sources.list
RUN install_packages acl ca-certificates curl gzip ldap-utils libaudit1 libbsd0 libc6 libcap-ng0 libedit2 libffi6 libgcc1 libgmp10 libgnutls30 libhogweed4 libicu63 libidn2-0 libldap-2.4-2 liblzma5 libnettle6 libnss-ldapd libp11-kit0 libpam-ldapd libpam0g libsasl2-2 libssl1.1 libstdc++6 libtasn1-6 libtinfo6 libunistring2 libuuid1 libxml2 libxslt1.1 nslcd procps tar zlib1g
COPY postgresql-client-10.16.0-0-linux-amd64-debian-10/files /opt/qingcloud
COPY wait-for-port-1.0.0-3-linux-amd64-debian-10/files /opt/qingcloud
COPY pgpool-4.2.2-0-linux-amd64-debian-10/files /opt/qingcloud
COPY gosu-1.12.0-2-linux-amd64-debian-10/files /opt/qingcloud
RUN chmod g+rwX /opt/qingcloud
COPY rootfs /
RUN /opt/qingcloud/scripts/pgpool/postunpack.sh
ENV QINGCLOUD_APP_NAME="pgpool" \
    QINGCLOUD_IMAGE_VERSION="4.2.2-debian-10-r72" \
    PATH="/opt/qingcloud/postgresql/bin:/opt/qingcloud/common/bin:/opt/qingcloud/pgpool/bin:$PATH" \
    POSTGRESQL_CLIENT_CREATE_DATABASE_NAME="" \
    POSTGRESQL_CLIENT_CREATE_DATABASE_PASSWORD="" \
    POSTGRESQL_CLIENT_CREATE_DATABASE_USERNAME="" \
    POSTGRESQL_HOST="postgresql" \
    POSTGRESQL_PORT_NUMBER="5432" \
    POSTGRESQL_ROOT_PASSWORD="" \
    POSTGRESQL_ROOT_USER="postgres"\
    LD_LIBRARY_PATH="/opt/qingcloud/pgpool/lib:/opt/qingcloud/postgresql/lib:$PATH"

EXPOSE 5432

USER 1001
ENTRYPOINT [ "/opt/qingcloud/scripts/pgpool/entrypoint.sh" ]
CMD [ "/opt/qingcloud/scripts/pgpool/run.sh" ]
