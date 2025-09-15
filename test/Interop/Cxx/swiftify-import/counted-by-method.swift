// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Method -source-filename=x | %FileCheck %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/method.swift -dump-macro-expansions -typecheck -verify

// CHECK: @_alwaysEmitIntoClient 
// CHECK-SAME: public mutating func bar(_ p: UnsafeMutableBufferPointer<Float>)

//--- Inputs/module.modulemap
module Method {
    header "method.h"
    requires cplusplus
}

//--- Inputs/method.h

class Foo {
public:
   __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))"))) void bar(float *p, int len);
};

//--- method.swift
import Method
  
func test(s: UnsafeMutableBufferPointer<Float>) {
  var foo = Foo()
  foo.bar(s)
}
