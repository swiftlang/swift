DIR="$( cd "$( dirname "$0" )" && pwd )"
$DIR/infer-imports.py -s $(xcrun --sdk macosx --show-sdk-path) > /tmp/modules-osx.txt && \
$DIR/infer-imports.py -s $(xcrun --sdk iphoneos --show-sdk-path) > /tmp/modules-iphoneos.txt && \
$DIR/infer-imports.py -s $(xcrun --sdk appletvos --show-sdk-path) > /tmp/modules-tvos.txt && \
$DIR/infer-imports.py -s $(xcrun --sdk watchos --show-sdk-path) > /tmp/modules-watchos.txt && \
cat $DIR/fixed-clang-modules-common.txt >> /tmp/modules-osx.txt && \
cat $DIR/fixed-clang-modules-macosx.txt >> /tmp/modules-osx.txt && \
cat $DIR/fixed-clang-modules-common.txt >> /tmp/modules-iphoneos.txt && \
cat $DIR/fixed-clang-modules-iphoneos.txt >> /tmp/modules-iphoneos.txt && \
cat $DIR/fixed-clang-modules-common.txt >> /tmp/modules-tvos.txt && \
cat $DIR/fixed-clang-modules-appletvos.txt >> /tmp/modules-tvos.txt && \
cat $DIR/fixed-clang-modules-common.txt >> /tmp/modules-watchos.txt && \
cat $DIR/fixed-clang-modules-watchos.txt >> /tmp/modules-watchos.txt
