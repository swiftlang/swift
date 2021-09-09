; This is not really a Swift source file: -*- Text -*-

RUN: swift-demangle < %S/Inputs/bigtype.txt 2>&1 > %t.check
RUN: %diff -u %S/Inputs/bigtype-demangle.txt %t.check

RUN: swift-demangle -remangle-new < %S/Inputs/bigtype.txt 2>&1 | sed -e 's/(\([0-9]\{1,\}\):[0-9]\{1,\})/(\1:xxx)/g' > %t.check 2>&1 || true
RUN: %diff -u %S/Inputs/bigtype-remangle.txt %t.check

RUN: swift-demangle -remangle-objc-rt < %S/Inputs/bigtype.txt 2>&1 | sed -e 's/(\([0-9]\{1,\}\):[0-9]\{1,\})/(\1:xxx)/g' > %t.check || true
RUN: %diff -u %S/Inputs/bigtype-objcrt.txt %t.check
