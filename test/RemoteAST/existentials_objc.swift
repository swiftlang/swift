// RUN: %target-swift-remoteast-test-with-sdk %s | %FileCheck %s

// REQUIRES: swift-remoteast-test
// REQUIRES: objc_interop

import Foundation

@_silgen_name("printDynamicTypeAndAddressForExistential")
func printDynamicTypeAndAddressForExistential<T>(_: T)

@_silgen_name("stopRemoteAST")
func stopRemoteAST()

// CHECK: NSObject
printDynamicTypeAndAddressForExistential(NSObject() as AnyObject)

// Print tagged pointer types three times to ensure the caching works.

// CHECK: NSNumber
printDynamicTypeAndAddressForExistential(NSNumber(123) as AnyObject)

// CHECK: NSNumber
printDynamicTypeAndAddressForExistential(NSNumber(123) as AnyObject)

// CHECK: NSNumber
printDynamicTypeAndAddressForExistential(NSNumber(123) as AnyObject)

// CHECK: NSString
printDynamicTypeAndAddressForExistential(NSString("hello") as AnyObject)

// CHECK: NSString
printDynamicTypeAndAddressForExistential(NSString("hello") as AnyObject)

// CHECK: NSString
printDynamicTypeAndAddressForExistential(NSString("hello") as AnyObject)

// Bridged NSError.
class ClassError : NSError {
  required init(coder: NSCoder) { fatalError() }
  init() {
    super.init(domain: "ClassError", code: 10, userInfo: [:])
  }
}

// CHECK: ClassError
printDynamicTypeAndAddressForExistential(ClassError() as Error)

stopRemoteAST()