#!/bin/bash

# Load environment variables from the .env file
export $(grep -v '^#' .env | xargs)

# Define the Docker command using the .env file
docker run -v C:/chinapat.onp/source/R_research_package/miniCRAN:/miniCRAN \
    -e R_VERSION_DATE="$R_VERSION_DATE" \
    --env-file="$ENV_FILE" \
    minicran
