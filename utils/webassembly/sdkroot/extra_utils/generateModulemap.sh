#!/bin/sh
exec sed -e "s@\"/include@\"$1/include@g" "$(dirname $0)/glibc.modulemap"
