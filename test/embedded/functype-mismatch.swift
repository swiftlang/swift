// RUN: not %target-swift-frontend %s -enable-experimental-feature Embedded -wmo -emit-sil -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx

// This is a mismatch compared to what's in the embedded stdlib
@_silgen_name("putchar")
// CHECK: type mismatch of function 'putchar'
public func putchar(_ value: CInt, _ x: Int) -> CInt {
  return 0
}

print("hello")
