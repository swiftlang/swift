// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -enable-objective-c-protocol-symbolic-references -target %target-future-triple %s -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import Foundation

@objc protocol TheObjCProtocol {}


func printMetadata<T>(_ t: T.Type) {
    print(t)
}

public func theTest() {
  printMetadata(TheObjCProtocol.self)
  print(ObjectIdentifier(NSObjectProtocol.self as AnyObject) == ObjectIdentifier(objc_getProtocol("NSObject")!))
  assert(ObjectIdentifier(NSObjectProtocol.self as AnyObject) == ObjectIdentifier(objc_getProtocol("NSObject")!))
}

theTest()

// CHECK: TheObjCProtocol
// CHECK: true
