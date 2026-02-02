// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library module

// SIL checking
// RUN: %target-swift-frontend %t/Library.swift -parse-as-library -entry-point-function-name Library_main -enable-experimental-feature Embedded -enable-experimental-feature DeferredCodeGen -emit-sil -emit-module-path %t/Modules/Library.swiftmodule -o - | %FileCheck -check-prefix LIBRARY-SIL %s

// RUN: %target-swift-frontend %t/Application.swift -I %t/Modules -parse-as-library -entry-point-function-name Application_main -enable-experimental-feature Embedded -emit-sil -o - | %FileCheck -check-prefix APPLICATION-SIL %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_DeferredCodeGen

//--- Library.swift

func internalFunc() { }

// LIBRARY-SIL: sil [asmname "swift_dosomething"] @$e7Library17swift_dosomethingyyFTo : $@convention(c) () -> () {
@c
public func swift_dosomething() {
  internalFunc()
}

//--- Application.swift
import Library

// APPLICATION-SIL-LABEL: sil @$e11Application4testyyF : $@convention(thin) () -> ()
public func test() {
  // CHECK: function_ref @$e7Library17swift_dosomethingyyFTo : $@convention(c) () -> ()
  swift_dosomething()
}
