// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Method -source-filename=x | %FileCheck %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -enable-experimental-feature SafeInteropWrappers %t/method.swift -dump-macro-expansions -typecheck -verify

// CHECK: class Foo {
// CHECK:  func bar(_ p: UnsafeMutableBufferPointer<Float>)

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h

@interface Foo
-(void)bar:(float *)p count:(int)len __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))")));
@end

//--- method.swift
import Method
  
func test(foo: Foo, s: UnsafeMutableBufferPointer<Float>) {
  foo.bar(s)
}    
