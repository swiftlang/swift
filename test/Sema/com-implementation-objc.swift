// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module-path %t/COM.swiftmodule -module-name COM -enable-experimental-com-interop %S/../Inputs/COM.swift
// RUN: %target-swift-frontend -typecheck -verify %t/main.swift -enable-experimental-com-interop -I %t

// REQUIRES: objc_interop

//--- module.modulemap
module ObjCRoot {
  header "ObjCRoot.h"
}

//--- ObjCRoot.h
__attribute__((__objc_root_class__))
@interface ObjCRoot
@end

//--- main.swift
import COM
import ObjCRoot

@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IInterface {}

class ObjCImplementation: ObjCRoot, IInterface {}
// expected-error@-1 {{class 'ObjCImplementation' cannot provide a COM implementation because it does not use the native Swift object model}}
