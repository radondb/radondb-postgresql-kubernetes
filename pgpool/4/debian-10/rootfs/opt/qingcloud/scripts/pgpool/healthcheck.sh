#!/bin/bash
#
# Qingcloud Pgpool healthcheck

# shellcheck disable=SC1091

set -o errexit
set -o nounset
set -o pipefail
# set -o xtrace # Uncomment this line for debugging purpose

# Load libraries
. /opt/qingcloud/scripts/libpgpool.sh

# Load Pgpool env. variables
eval "$(pgpool_env)"

pgpool_healthcheck
