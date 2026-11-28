// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t%{fs-sep}use-cxx-types.swift -module-name UseCxx -typecheck -verify -emit-clang-header-path %t%{fs-sep}UseCxx.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public
// RUN: %FileCheck --check-prefix=CHECK-%target-ptrsize %s < %t%{fs-sep}UseCxx.h
// RUN: %if OS=macosx %{ %FileCheck --check-prefix=CHECK-MACOSX %s < %t%{fs-sep}UseCxx.h %}

// RUN: %target-interop-build-clangxx -std=c++20 -c %t%{fs-sep}use-swift-cxx-types.cpp -I %t -o %t%{fs-sep}swift-cxx-execution.o
// RUN: %target-interop-build-swift %t%{fs-sep}use-cxx-types.swift -o %t%{fs-sep}swift-cxx-execution -Xlinker %t%{fs-sep}swift-cxx-execution.o -module-name UseCxx -Xfrontend -entry-point-function-name -Xfrontend swiftMain -I %t

// RUN: %target-codesign %t%{fs-sep}swift-cxx-execution
// RUN: %target-run %t%{fs-sep}swift-cxx-execution | %FileCheck %s

// REQUIRES: executable_test

//--- header.h

#include <cstdint>
#include <optional>
struct Inner {
  int64_t a;
  bool b;
};

// `c` is after the tail padding of `Inner`, at offset 16. Without `aligned`,
// the stub would put it at offset 9, in the same chunk as `inner.b`.
struct Outer {
  Inner inner;
  bool c;
};

struct alignas(4) Empty4 {};

// The first field is not at offset 0.
struct EmptyAndUInt8 {
  Empty4 e;
  uint8_t a;
  Empty4 f;
  uint8_t b;
};

struct Empty1 {};

// `b` is at offset 5, and the stub puts it at offset 4. Both are in the first
// chunk, so only `c` needs to be moved to the next chunk.
struct FloatEmptyAndUInt8 {
  float a;
  Empty1 e;
  uint8_t b;
  alignas(8) uint8_t c;
};

enum class A : uint8_t { x, y };
struct FloatSize { float w = 0, h = 0; };

// `e` and `f` are after the padding of the optional, at offset 16.
struct Opts {
  A a = A::x, b = A::x, c = A::x, d = A::x;
  std::optional<FloatSize> size;
  A e = A::x;
  A f = A::x;
};

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public func printOuter(_ x: Outer) { print("Outer(\(x.inner.a), \(x.inner.b), \(x.c))") }
public func passThroughOuter(_ x: Outer) -> Outer { return x }

public func printEmptyAndUInt8(_ x: EmptyAndUInt8) { print("EmptyAndUInt8(\(x.a), \(x.b))") }
public func passThroughEmptyAndUInt8(_ x: EmptyAndUInt8) -> EmptyAndUInt8 { return x }

public func printFloatEmptyAndUInt8(_ x: FloatEmptyAndUInt8) {
  print("FloatEmptyAndUInt8(\(x.a), \(x.b), \(x.c))")
}
public func passThroughFloatEmptyAndUInt8(_ x: FloatEmptyAndUInt8) -> FloatEmptyAndUInt8 { return x }

public func printOpts(_ x: Opts) { print("Opts(\(x.e.rawValue), \(x.f.rawValue))") }
public func passThroughOpts(_ x: Opts) -> Opts { return x }

// EmptyAndUInt8
// CHECK-64:      struct swift_interop_passStub_UseCxx_uint8_t_4_5_uint8_t_12_13 {
// CHECK-64-NEXT:   uint8_t _1;
// CHECK-64-NEXT:   uint8_t _2 __attribute__((aligned(8)));
// CHECK-64-NEXT: };

// CHECK-32:      struct swift_interop_passStub_UseCxx_uint8_t_4_5_uint8_t_12_13 {
// CHECK-32-NEXT:   uint8_t _1;
// CHECK-32-NEXT:   uint8_t _2 __attribute__((aligned(4)));
// CHECK-32-NEXT: };

// FloatEmptyAndUInt8
// CHECK-64:      struct swift_interop_passStub_UseCxx_float_0_4_uint8_t_5_6_uint8_t_8_9 {
// CHECK-64-NEXT:   float _1;
// CHECK-64-NEXT:   uint8_t _2;
// CHECK-64-NEXT:   uint8_t _3 __attribute__((aligned(8)));
// CHECK-64-NEXT: };

// Opts is only passed directly on macOS.
// CHECK-MACOSX:      struct swift_interop_passStub_UseCxx_uint64_t_0_8_float_8_12_bool_12_13_uint16_t_16_18 {
// CHECK-MACOSX-NEXT:   uint64_t _1;
// CHECK-MACOSX-NEXT:   float _2;
// CHECK-MACOSX-NEXT:   bool _3;
// CHECK-MACOSX-NEXT:   uint16_t _4 __attribute__((aligned(8)));
// CHECK-MACOSX-NEXT: };

// Outer
// CHECK-64:      struct swift_interop_returnStub_UseCxx_uint64_t_0_8_bool_8_9_bool_16_17 {
// CHECK-64-NEXT:   uint64_t _1;
// CHECK-64-NEXT:   bool _2;
// CHECK-64-NEXT:   bool _3 __attribute__((aligned(8)));
// CHECK-64-NEXT: };

//--- use-swift-cxx-types.cpp

#include "header.h"
#include "UseCxx.h"

int main() {
  {
    Outer x{{-1, false}, true};
    UseCxx::printOuter(x);
    UseCxx::printOuter(UseCxx::passThroughOuter(x));
  }
// CHECK:      Outer(-1, false, true)
// CHECK-NEXT: Outer(-1, false, true)
  {
    EmptyAndUInt8 x{{}, 7, {}, 9};
    UseCxx::printEmptyAndUInt8(x);
    UseCxx::printEmptyAndUInt8(UseCxx::passThroughEmptyAndUInt8(x));
  }
// CHECK-NEXT: EmptyAndUInt8(7, 9)
// CHECK-NEXT: EmptyAndUInt8(7, 9)
  {
    FloatEmptyAndUInt8 x{1.5f, {}, 7, 9};
    UseCxx::printFloatEmptyAndUInt8(x);
    UseCxx::printFloatEmptyAndUInt8(UseCxx::passThroughFloatEmptyAndUInt8(x));
  }
// CHECK-NEXT: FloatEmptyAndUInt8(1.5, 7, 9)
// CHECK-NEXT: FloatEmptyAndUInt8(1.5, 7, 9)
  {
    Opts x;
    x.e = A::y;
    x.f = A::y;
    x.size = FloatSize{1.5f, 2.5f};
    UseCxx::printOpts(x);
    UseCxx::printOpts(UseCxx::passThroughOpts(x));
  }
// CHECK-NEXT: Opts(1, 1)
// CHECK-NEXT: Opts(1, 1)
  return 0;
}
