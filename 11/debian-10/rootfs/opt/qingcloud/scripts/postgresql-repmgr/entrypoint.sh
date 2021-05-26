#!/bin/bash

# shellcheck disable=SC1091

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

# Load libraries
. /opt/qingcloud/scripts/liblog.sh
. /opt/qingcloud/scripts/libqingcloud.sh
. /opt/qingcloud/scripts/libpostgresql.sh
. /opt/qingcloud/scripts/librepmgr.sh

# Load PostgreSQL & repmgr environment variables
. /opt/qingcloud/scripts/postgresql-env.sh
export MODULE=postgresql-repmgr

# print_welcome_page

# Enable the nss_wrapper settings
postgresql_enable_nss_wrapper

if [[ "$*" = *"/opt/qingcloud/scripts/postgresql-repmgr/run.sh"* ]]; then
    info "** Starting PostgreSQL with Replication Manager setup **"
    /opt/qingcloud/scripts/postgresql-repmgr/setup.sh
    touch "$POSTGRESQL_TMP_DIR"/.initialized
    info "** PostgreSQL with Replication Manager setup finished! **"
fi

echo ""
exec "$@"
