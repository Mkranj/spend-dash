#!/bin/bash

# go to root
cd ..
LOCAL_WD=$(pwd)

TARGET_DIR="$LOCAL_WD"/"test_copy/"

mkdir "$TARGET_DIR"
# Copy all source files to new folder

cp {app.R,compose.yaml,dockerfile,env_variables.R,renv.lock,ui_definition.R} "$TARGET_DIR"
cp -r {data_files,functions,modules,nginx,scripts,www} "$TARGET_DIR"