#!/bin/bash

set -xe
repository='swiftwasm/swift'
workflow_name='main.yml'
branch='swiftwasm'

gh_api=https://api.github.com

github() {
  curl --header "authorization: Bearer $GITHUB_TOKEN" "$@"
}

latest_run=$(github "${gh_api}/repos/${repository}/actions/workflows/${workflow_name}/runs?branch=${branch}&status=completed" | jq '.workflow_runs | sort_by(.run_number) | last')
artifacts_url=$(echo $latest_run | jq .artifacts_url --raw-output)
head_sha=$(echo $latest_run | jq .head_sha --raw-output)


get_artifact_url() {
  local name=$1
  github $artifacts_url --fail | jq ".artifacts[] | select(.name == \"$name\") | .archive_download_url" | sed 's/\"//g'
}

download_artifact() {
  local name=$1
  github -L "$(get_artifact_url $name)" --fail -o "$name.zip"
}

is_released() {
  local name=$1
  local code=$(github "$gh_api/repos/$repository/releases/tags/$name" -o /dev/null -w '%{http_code}')
  test $code = 200
}

create_tag() {
  local name=$1
  local sha=$2
  local body=$(cat <<EOS
    {
      "tag": "$name",
      "message": "$name",
      "object": "$sha",
      "type": "commit"
    }
EOS
)
  github --request POST --fail \
    --url "${gh_api}/repos/${repository}/git/tags" \
    --data "$body"
}

create_release() {
  local name=$1
  local tag=$2
  local sha=$3
  local body=$(cat <<EOS
    {
      "tag_name": "$tag",
      "target_commitish": "$sha",
      "name": "$name",
      "prerelease": true
    }
EOS
)
  local response=$(github \
    --request POST --fail \
    --url "${gh_api}/repos/${repository}/releases" \
    --data "$body")
  echo $response | jq .id
}

upload_tarball() {
  local release_id=$1
  local artifact=$2
  local filename=$(basename $artifact)

  github -XPOST --fail \
    -H "Content-Length: $(stat -f%z "$artifact")" \
    -H "Content-Type: application/x-gzip" \
    --upload-file "$artifact" \
    "https://uploads.github.com/repos/$repository/releases/$release_id/assets?name=$filename"
}

tmp_dir=$(mktemp -d)
pushd $tmp_dir
download_artifact linux-installable
download_artifact macos-installable
unzip linux-installable.zip
unzip macos-installable.zip

toolchain_name=$(basename $(tar tfz swift-wasm-DEVELOPMENT-SNAPSHOT-linux.tar.gz | head -n1))

if is_released $toolchain_name; then
  echo "Latest toolchain $toolchain_name has been already released"
  exit 0
fi

cp swift-wasm-DEVELOPMENT-SNAPSHOT-linux.tar.gz "$toolchain_name-linux.tar.gz"
cp swift-wasm-DEVELOPMENT-SNAPSHOT-osx.tar.gz "$toolchain_name-osx.tar.gz"

create_tag $toolchain_name $head_sha
release_id=$(create_release $toolchain_name $toolchain_name $head_sha)

upload_tarball $release_id "$toolchain_name-osx.tar.gz"
upload_tarball $release_id "$toolchain_name-linux.tar.gz"

popd
