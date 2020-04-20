set -x

SOURCE_DIR="$(cd "$(dirname $0)/../../.." && pwd)"
DESTINATION="${SOURCE_DIR}/swift-nightly-toolchain"

mkdir -p $DESTINATION

install_linux() {
  WORKSPACE=$(mktemp -d)

  pushd ${WORKSPACE}

  BASE_URL=https://swift.org/builds/development/ubuntu1804
  export $(/usr/bin/curl ${BASE_URL}/latest-build.yml | grep 'download:' | sed 's/:[^:\/\/]/=/g')

  DOWNLOAD_DIR=$(echo $download | sed "s/-ubuntu18.04.tar.gz//g")
  DOWNLOAD_URL=${BASE_URL}/${DOWNLOAD_DIR}/${download}
  /usr/bin/curl ${BASE_URL}/${DOWNLOAD_DIR}/${download} > ${WORKSPACE}/latest_toolchain.tar.gz

  mkdir -p ${WORKSPACE}/latest_toolchain
  tar xzf ${WORKSPACE}/latest_toolchain.tar.gz -C ${WORKSPACE}/latest_toolchain
  mv ${WORKSPACE}/latest_toolchain/${DOWNLOAD_DIR}-ubuntu18.04/usr ${DESTINATION}/usr

  popd
}

install_macos() {
  WORKSPACE=$(mktemp -d)

  pushd ${WORKSPACE}

  BASE_URL=https://swift.org/builds/development/
  export $(/usr/bin/curl --silent ${BASE_URL}/xcode/latest-build.yml | grep 'download:' | sed 's/:[^:\/\/]/=/g')

  DOWNLOAD_DIR=$(echo $download | sed "s/-osx.pkg//g")
  DOWNLOAD_URL=${BASE_URL}/xcode/${DOWNLOAD_DIR}/${download}

  /usr/bin/curl --silent ${DOWNLOAD_URL} > ${WORKSPACE}/latest_xcode_toolchain.pkg
  pkgutil --expand ${WORKSPACE}/latest_xcode_toolchain.pkg ${WORKSPACE}/latest_xcode_toolchain
  tar xf latest_xcode_toolchain/${DOWNLOAD_DIR}-osx-package.pkg/Payload
  mv ${WORKSPACE}/usr ${DESTINATION}/usr

  popd
}

if [[ "$(uname)" == "Linux" ]]; then
  install_linux
else
  install_macos
fi
