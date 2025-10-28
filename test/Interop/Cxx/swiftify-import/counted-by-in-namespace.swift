// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/namespace.swift -emit-module -verify
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Namespace -source-filename=x > %t/interface.txt
// RUN: diff %t/interface.txt %t/interface.txt.expected

//--- interface.txt.expected
@_exported import Namespace.Baz

enum foo {
  static func foo_func(_ p: UnsafeMutablePointer<Float>!, _ len: Int32)
  /// This is an auto-generated wrapper for safer interop
  @_alwaysEmitIntoClient @_disfavoredOverload public static func foo_func(_ p: UnsafeMutableBufferPointer<Float>)
  static func foo_func2(_ p: UnsafePointer<baz_t>!, _ len: baz_t)
  /// This is an auto-generated wrapper for safer interop
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
  @_alwaysEmitIntoClient @_disfavoredOverload public static func foo_func2(_ p: Span<baz_t>)
  enum bar {
    static func bar_func(_ p: UnsafePointer<baz_t>!, _ len: baz_t)
    /// This is an auto-generated wrapper for safer interop
    @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_alwaysEmitIntoClient @_disfavoredOverload public static func bar_func(_ p: Span<baz_t>)
  }
}
//--- Inputs/module.modulemap
module Namespace {
    header "namespace.h"
    requires cplusplus
    module Baz {
      header "baz.h"
    }
}

//--- Inputs/namespace.h
#include "baz.h"

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

namespace foo {
  __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))"))) void foo_func(float *p, int len);
  void foo_func2(const baz_t * __counted_by(len) p __noescape, baz_t len);
  namespace bar {
    void bar_func(const baz_t * __counted_by(len) p __noescape, baz_t len);
  }
}

//--- Inputs/baz.h
typedef int baz_t;

//--- namespace.swift
import Namespace

func test(s: UnsafeMutableBufferPointer<Float>) {
  foo.foo_func(s)
}

func test2(s: Span<CInt>) {
  foo.foo_func2(s)
}

func test3(s: Span<CInt>) {
  foo.bar.bar_func(s)
}
