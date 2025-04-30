// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: rm -rf %t
// RUN: split-file %s %t
// RUN: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=upcoming-swift -enable-experimental-feature SafeInteropWrappers -print-module -module-to-print=Namespace -source-filename=x | %FileCheck %s
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/namespace.swift -dump-macro-expansions -typecheck -verify

// CHECK: enum foo {
// CHECK:  static func bar(_ p: UnsafeMutableBufferPointer<Float>)

//--- Inputs/module.modulemap
module Namespace {
    header "namespace.h"
    requires cplusplus
}

//--- Inputs/namespace.h

namespace foo {
   __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))"))) void bar(float *p, int len);
}

//--- namespace.swift
import Namespace
  
func test(s: UnsafeMutableBufferPointer<Float>) {
  foo.bar(s)
}    
