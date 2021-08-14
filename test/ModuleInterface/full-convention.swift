// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -swift-version 5 -emit-module-interface-path - -enable-library-evolution %s -experimental-print-full-convention -use-clang-function-types | %FileCheck %s

import ctypes

public func f(
  // CHECK: g: @convention(c)
  g: @convention(c) () -> (),

  // CHECK: h0: @convention(c)
  h0: @convention(c) (Int64) -> Int32,
  // CHECK: h1: @convention(c)
  h1: @convention(c, cType: "int (*)(long long)") (Int64) -> Int32,

  // CHECK: h1c: @convention(c, cType: "intptr_t (*)(size_t)")
  h1c: @convention(c, cType: "intptr_t (*)(size_t)") (Int) -> Int,

  // CHECK: i0: @convention(c)
  i0: @convention(c) (Int64, Int32) -> Optional<UnsafeMutablePointer<Int32>>,
  // CHECK: i1: @convention(c)
  i1: @convention(c, cType: "int *(*)(long long, int)") (Int64, Int32) -> Optional<UnsafeMutablePointer<Int32>>,

  // CHECK: i1c: @convention(c, cType: "size_t *(*)(intptr_t, ptrdiff_t)")
  i1c: @convention(c, cType: "size_t *(*)(intptr_t, ptrdiff_t)") (Int, Int) -> Optional<UnsafeMutablePointer<Int>>,

  // CHECK: p0: @convention(c)
  // CHECK:     @convention(c)
  p0: @convention(c) (@convention(c) (Int32) -> Void) -> Void,

  // CHECK: p1: @convention(c)
  // CHECK:     @convention(c)
  p1: @convention(c, cType: "void (*)(void (*)(int))") (@convention(c) (Int32) -> Void) -> Void,

  // CHECK: p1c: @convention(c, cType: "void (*)(void (*)(size_t))")
  // CHECK:      @convention(c)
  p1c: @convention(c, cType: "void (*)(void (*)(size_t))") (@convention(c) (Int) -> Void) -> Void,

  // CHECK: p2: @convention(c)
  // CHECK:     @convention(c)
  p2: @convention(c) (@convention(c, cType: "void (*)(int)") (Int32) -> Void) -> Void,

  // CHECK: p2c: @convention(c)
  // CHECK:      @convention(c, cType: "void (*)(size_t)")
  p2c: @convention(c) (@convention(c, cType: "void (*)(size_t)") (Int) -> Void) -> Void,

  // CHECK: p3: @convention(c)
  // CHECK:     @convention(c)
  p3: @convention(c, cType: "void (*)(void (*)(int))") (@convention(c, cType: "void (*)(int)") (Int32) -> Void) -> Void,

  // CHECK: p3c: @convention(c)
  // CHECK:      @convention(c, cType: "void (*)(size_t)")
  p3c: @convention(c, cType: "void (*)(void (*)(size_t))") (@convention(c, cType: "void (*)(size_t)") (Int) -> Void) -> Void
) {}
