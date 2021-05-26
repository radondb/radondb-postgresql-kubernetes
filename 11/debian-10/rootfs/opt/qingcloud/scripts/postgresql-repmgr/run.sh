#!/bin/bash

# shellcheck disable=SC1091

set -o errexit
set -o nounset
set -o pipefail
# set -o xtrace # Uncomment this line for debugging purpose

# Load libraries
. /opt/qingcloud/scripts/liblog.sh
. /opt/qingcloud/scripts/libpostgresql.sh
. /opt/qingcloud/scripts/librepmgr.sh

# Load PostgreSQL & repmgr environment variables
. /opt/qingcloud/scripts/postgresql-env.sh

readonly repmgr_flags=("--pid-file=$REPMGR_PID_FILE" "-f" "$REPMGR_CONF_FILE" "--daemonize=false")
readonly repmgr_cmd=$(command -v repmgrd)
postgresql_start_bg true
info "** Starting repmgrd **"
# TODO: properly test running the container as root
if am_i_root; then
    exec gosu "$POSTGRESQL_DAEMON_USER" "$repmgr_cmd" "${repmgr_flags[@]}"
else
    exec "$repmgr_cmd" "${repmgr_flags[@]}"
fi
