// REQUIRES: swift_feature_SafeInteropWrappers

// FIXME swift-ci linux tests do not support std::span
// UNSUPPORTED: OS=linux-gnu, OS=linux-android, OS=linux-androideabi

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-frontend -c -plugin-path %swift-plugin-dir -I %t/Inputs -Xcc -std=c++20 -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/method.swift -dump-macro-expansions -verify 2>&1 | %FileCheck %s

// CHECK: @_alwaysEmitIntoClient 
// CHECK: public init(_ sp: Span<CInt>) {
// CHECK:    unsafe self.init(IntSpan(sp))
// CHECK: }


//--- Inputs/module.modulemap
module Method {
    header "method.h"
    requires cplusplus
}

//--- Inputs/method.h

#include <span>

using IntSpan = std::span<const int>;

class Foo {
public:
   Foo();
   Foo(IntSpan sp [[clang::noescape]]);
};

//--- method.swift
import CxxStdlib
import Method
  
func test(s: Span<Int32>) {
  var _ = Foo(s)
}
