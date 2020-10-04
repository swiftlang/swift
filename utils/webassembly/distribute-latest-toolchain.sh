#!/bin/bash

set -xe
repository='swiftwasm/swift'
workflow_name='main.yml'
branch=$1

gh_api=https://api.github.com

github() {
  curl --header "authorization: Bearer $GITHUB_TOKEN" "$@"
}

latest_run=$(github "${gh_api}/repos/${repository}/actions/workflows/${workflow_name}/runs?head_branch=${branch}&status=completed&conclusion=success" \
  | jq ".workflow_runs | map(select(.head_branch == \"$branch\")) | sort_by(.run_number) | last")

if [ -z "$latest_run" ] || [ "$latest_run" == "null" ]; then
  echo "No successful runs available"
  exit 0
fi

artifacts_url=$(echo $latest_run | jq .artifacts_url --raw-output)
head_sha=$(echo $latest_run | jq .head_sha --raw-output)

get_artifact_url() {
  local name=$1
  github $artifacts_url --fail | jq ".artifacts[] | select(.name == \"$name\") | .archive_download_url" | sed 's/\"//g'
}

download_artifact() {
  local name=$1
  local artifact_url="$(get_artifact_url $name)"

  if [ -z "$artifact_url" ] || [ "$artifact_url" == "null" ]; then
    echo "No successfully built artifacts available for $name"
    exit 0
  fi

  github -L "$artifact_url" --fail -o "$name.zip"
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
download_artifact ubuntu18.04-installable
download_artifact ubuntu20.04-installable
download_artifact macos-installable
unzip ubuntu18.04-installable.zip
unzip ubuntu20.04-installable.zip
unzip macos-installable.zip

toolchain_name=$(basename $(tar tfz swift-wasm-$2-SNAPSHOT-ubuntu18.04.tar.gz | head -n1))

if is_released $toolchain_name; then
  echo "Latest toolchain $toolchain_name has been already released"
  exit 0
fi

mv swift-wasm-$2-SNAPSHOT-ubuntu18.04.tar.gz "$toolchain_name-ubuntu18.04-x86_64.tar.gz"
mv swift-wasm-$2-SNAPSHOT-ubuntu20.04.tar.gz "$toolchain_name-ubuntu20.04-x86_64.tar.gz"
mv swift-wasm-$2-SNAPSHOT-osx.tar.gz "$toolchain_name-macos-x86_64.tar.gz"

create_tag $toolchain_name $head_sha
release_id=$(create_release $toolchain_name $toolchain_name $head_sha)

upload_tarball $release_id "$toolchain_name-ubuntu18.04.tar.gz"
upload_tarball $release_id "$toolchain_name-ubuntu20.04.tar.gz"
upload_tarball $release_id "$toolchain_name-macos-x86_64.tar.gz"

popd
