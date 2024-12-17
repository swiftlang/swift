MODE=$1
TEST=$2

pushd ../build/${MODE}/swift-macosx-arm64
xcrun ninja -j 8 swift-frontend
#xcrun ninja -j 8 swift-refactor
#xcrun ninja swift-demangle
#xcrun ninja swift-serialize-diagnostics
#xcrun ninja sil-opt
#xcrun ninja swift-ide-test
#xcrun ninja sourcekitd-test
#xcrun ninja stdlib
#xcrun ninja swift-remoteast
popd
python3 ../llvm-project/llvm/utils/lit/lit.py -sv --param swift_site_config=../build/${MODE}/swift-macosx-arm64/test-macosx-arm64/lit.site.cfg ${TEST}
