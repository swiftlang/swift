// RUN: %target-swift-frontend %s -target %target-cpu-apple-macos14 -emit-ir -g -Onone -enable-experimental-feature Embedded -wmo -disable-availability-checking -o - | %FileCheck %s

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// An embedded key path is a constant global rather than the result of a runtime
// call, so a variable bound to one is backed by an llvm::Constant. A dbg_value
// naming a global has no DWARF location at -Onone, so it needs a stack slot.

public func f() -> [Int] {
  struct S {
    var values: [[Int]] = [[1, 2]]
  }
  let kp = \S.values
  let s = S()
  print(s[keyPath: kp][0][0])
  return [0]
}

// CHECK: %kp.debug = alloca ptr
// CHECK: #dbg_declare(ptr %kp.debug, ![[KP:[0-9]+]], !DIExpression()
// CHECK: store ptr @keypath, ptr %kp.debug
// CHECK-DAG: ![[KP]] = !DILocalVariable(name: "kp"
