// RUN: not %target-swift-frontend -typecheck -verify %s 2>&1 | %FileCheck %s
// REQUIRES: OS=windows-msvc && CPU=aarch64

import vcruntime
typealias JumpBuffer = _JBTYPE

func test_unavailable_returns_twice_function() {
  var x: JumpBuffer
  _ = _setjmp(&x)
}

// CHECK: availability_returns_twice-msvc-aarch64.swift:{{[0-9]+}}:{{[0-9]+}}: error: unexpected error produced: cannot find type '_JBTYPE' in scope
// CHECK-NEXT: typealias JumpBuffer = _JBTYPE
// CHECK-NEXT:  ^
// CHECK-NEXT: setjmp.h:{{[0-9]+}}:{{[0-9]+}}: note: diagnostic produced elsewhere: macro '_JBTYPE' unavailable: structure not supported
// CHECK-NEXT:   #define _JBTYPE unsigned __int64

