#!/bin/bash

# Gather secrets
source ./envir.sh
echo "$REMOTE_SERVER"

# Go to project root
cd ..
LOCAL_WD=$(pwd)

TARGET_DIR="/SP_app"

# Copy all source files to new folder

#cp {app.R,compose.yaml,dockerfile,env_variables.R,renv.lock,ui_definition.R} "$TARGET_DIR"
#cp -r {data_files,functions,modules,nginx,scripts,www} "$TARGET_DIR"

pscp -pw $REMOTE_SERVER_PASS {app.R,compose.yaml,dockerfile,env_variables.R,renv.lock,ui_definition.R} $REMOTE_SERVER:$TARGET_DIR
pscp -pw $REMOTE_SERVER_PASS -r {data_files,functions,modules,nginx,scripts,www} $REMOTE_SERVER:$TARGET_DIR
