; This is not really a Swift source file: -*- Text -*-

; We need sed, so Windows is out
UNSUPPORTED: OS=windows-msvc

RUN: swift-demangle < %S/Inputs/bigtype.txt 2>&1 > %t.check
RUN: %diff -u %S/Inputs/bigtype-demangle.txt %t.check

RUN: swift-demangle -remangle-new < %S/Inputs/bigtype.txt 2>&1 | sed 's/([0-9]*:[0-9]*)/(pos)/g'  > %t.check || true
RUN: %diff -u %S/Inputs/bigtype-remangle.txt %t.check

RUN: swift-demangle -remangle-objc-rt < %S/Inputs/bigtype.txt 2>&1 | sed 's/([0-9]*:[0-9]*)/(pos)/g' > %t.check || true
RUN: %diff -u %S/Inputs/bigtype-objcrt.txt %t.check

