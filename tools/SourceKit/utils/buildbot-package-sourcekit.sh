
# Make sure the variables and directories we expect to exist actually do.
test "$WORKSPACE"
test -d "$WORKSPACE/swift/build"
test -d "$WORKSPACE/SourceKit/build"

DATE_TAG=`date +%y%m%d%H%M`

mkdir -p SourceKit && cd SourceKit
mkdir -p bin && mkdir -p lib
cp "$WORKSPACE/SourceKit/build/bin/sourcekitd-test" bin
cp "$WORKSPACE/SourceKit/build/bin/sourcekitd-repl" bin
cp "$WORKSPACE/SourceKit/build/lib/libsourcekitdInProc.dylib" lib
ditto "$WORKSPACE/SourceKit/build/lib/sourcekitd.framework" lib/sourcekitd.framework
cp -r "$WORKSPACE/swift/build/lib/swift" lib
ditto "$WORKSPACE/llvm/build/lib/clang" lib/clang
cp "$WORKSPACE/llvm/build/lib/libclang.dylib" lib
install_name_tool -id @rpath/libclang.dylib lib/libclang.dylib
cd ..
tar -cvzf SourceKit-$DATE_TAG.tar.gz SourceKit
