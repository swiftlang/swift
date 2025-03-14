// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUNx: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Method -source-filename=x | %FileCheck %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/method.swift -dump-macro-expansions -typecheck -verify

// CHECK: @_alwaysEmitIntoClient 
// CHECK:  public mutating func bar(_ p: UnsafeMutableBufferPointer<Int32>)

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
