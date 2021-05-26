#!/bin/bash
#
# Qingcloud Pgpool entrypoint

# shellcheck disable=SC1091

set -o errexit
set -o nounset
set -o pipefail
#set -o xtrace

# Load libraries
. /opt/qingcloud/scripts/libqingcloud.sh
. /opt/qingcloud/scripts/liblog.sh
. /opt/qingcloud/scripts/libpgpool.sh

# Load Pgpool env. variables
eval "$(pgpool_env)"

# print_welcome_page

if [[ "$*" = *"/opt/qingcloud/scripts/pgpool/run.sh"* ]]; then
    info "** Starting Pgpool-II setup **"
    /opt/qingcloud/scripts/pgpool/setup.sh
    info "** Pgpool-II setup finished! **"
fi

echo ""
exec "$@"
