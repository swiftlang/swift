// RUN: %target-swift-frontend -typecheck -swift-version 5 -emit-module-interface-path - -enable-library-evolution %s -experimental-print-full-convention | %FileCheck %s

public func f(
  // CHECK: g: @convention(c, cType: "void (*)(void)")
  g: @convention(c) () -> (),

  // CHECK: h0: @convention(c, cType: "int (*)(long long)")
  h0: @convention(c) (Int64) -> Int32,
  // CHECK: h1: @convention(c, cType: "int (*)(long long)")
  h1: @convention(c, cType: "int (*)(long long)") (Int64) -> Int32,

  // CHECK: i0: @convention(c, cType: "int *(*)(long long, int)")
  i0: @convention(c) (Int64, Int32) -> Optional<UnsafeMutablePointer<Int32>>,
  // CHECK: i1: @convention(c, cType: "int *(*)(long long, int)")
  i1: @convention(c, cType: "int *(*)(long long, int)") (Int64, Int32) -> Optional<UnsafeMutablePointer<Int32>>,

  // CHECK: p0: @convention(c, cType: "void (*)(void (*)(int))")
  // CHECK:     @convention(c, cType: "void (*)(int)")
  p0: @convention(c) (@convention(c) (Int32) -> Void) -> Void,

  // CHECK: p1: @convention(c, cType: "void (*)(void (*)(int))")
  // CHECK:     @convention(c, cType: "void (*)(int)")
  p1: @convention(c, cType: "void (*)(void (*)(int))") (@convention(c) (Int32) -> Void) -> Void,

  // CHECK: p2: @convention(c, cType: "void (*)(void (*)(int))")
  // CHECK:     @convention(c, cType: "void (*)(int)")
  p2: @convention(c) (@convention(c, cType: "void (*)(int)") (Int32) -> Void) -> Void,

  // CHECK: p3: @convention(c, cType: "void (*)(void (*)(int))")
  // CHECK:     @convention(c, cType: "void (*)(int)")
  p3: @convention(c, cType: "void (*)(void (*)(int))") (@convention(c, cType: "void (*)(int)") (Int32) -> Void) -> Void
) {}
