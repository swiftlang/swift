#!/bin/sh
# A helper script that only installs or uninstalls a Swift package built
# by the buildbot. The _jenkins bot should be allowed to sudo this.

# Must be root to install packages.
if [ "$(id -u)" != 0 ]; then
  echo "Must install package as root!"
  exit 1
fi

MODE="$1"
PACKAGE="$2"

if [ \! "$PACKAGE" ]; then
  echo "No package name! Usage: $0 [install|uninstall] package.tar.gz"
  exit 1
fi

case "$MODE" in
install)
  darwinup install "$PACKAGE"
  exit $?
  ;;
uninstall)
  darwinup uninstall "$(basename "$PACKAGE")"
  exit $?
  ;;
*)
  echo "Mode must be install or uninstall!"
  echo "Usage: $0 [install|uninstall] package.tar.gz"
  exit 1
  ;;
esac

