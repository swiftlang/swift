; This is not really a Swift source file: -*- Text -*-

; We need sed and awk, so Windows is out
UNSUPPORTED: OS=windows-msvc

RUN: swift-demangle < %S/Inputs/bigtype.txt 2>&1 > %t.check
RUN: %diff -u %S/Inputs/bigtype-demangle.txt %t.check

RUN: swift-demangle -remangle-new < %S/Inputs/bigtype.txt 2>&1 | sed 's/([0-9]*:[0-9]*)/(pos)/g'  > %t.check || true
RUN: %diff -u %S/Inputs/bigtype-remangle.txt %t.check

RUN: swift-demangle -remangle-objc-rt < %S/Inputs/bigtype.txt 2>&1 | sed 's/([0-9]*:[0-9]*)/(pos)/g' > %t.check || true
RUN: %diff -u %S/Inputs/bigtype-objcrt.txt %t.check

; The names above trip mangle()'s own depth limit. A long class context chain
; reaches the substitution hashing and comparison walks, which have their own
; depth limit. limit does not cover. The trailing Protocol node keeps
; TypeDecoder's depth cap from rejecting the chain first.

RUN: awk 'BEGIN{ ORS=""; print "$s1M"; for (i = 0; i < 300000; i++) print "1aC"; print "4ProtP\n" }' > %t.deep
RUN: swift-demangle -remangle-new < %t.deep > %t.deep.check 2>&1 || true
RUN: %FileCheck %s < %t.deep.check
CHECK: unable to re-mangle

