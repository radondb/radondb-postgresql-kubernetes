#!/bin/bash

# shellcheck disable=SC1091

# Load libraries
. /opt/qingcloud/scripts/libfs.sh
. /opt/qingcloud/scripts/libpostgresql.sh

. /opt/qingcloud/scripts/librepmgr.sh

# Load PostgreSQL & repmgr environment variables
. /opt/qingcloud/scripts/postgresql-env.sh

for dir in "$POSTGRESQL_INITSCRIPTS_DIR" "$POSTGRESQL_TMP_DIR" "$POSTGRESQL_LOG_DIR" "$POSTGRESQL_CONF_DIR" "${POSTGRESQL_CONF_DIR}/conf.d" "$POSTGRESQL_MOUNTED_CONF_DIR" "${POSTGRESQL_MOUNTED_CONF_DIR}/conf.d" "$POSTGRESQL_VOLUME_DIR" "$REPMGR_CONF_DIR" "$REPMGR_TMP_DIR"; do
    ensure_dir_exists "$dir"
    chmod -R g+rwX "$dir"
done


# Copying events handlers
mv /events "$REPMGR_EVENTS_DIR"
chmod +x "$REPMGR_EVENTS_DIR"/router.sh "$REPMGR_EVENTS_DIR"/execs/*sh "$REPMGR_EVENTS_DIR"/execs/includes/*sh

# Redirect all logging to stdout
ln -sf /dev/stdout "$POSTGRESQL_LOG_FILE"
