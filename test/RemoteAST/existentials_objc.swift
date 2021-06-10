// RUN: %target-swift-remoteast-test-with-sdk %s | %FileCheck %s

// REQUIRES: swift-remoteast-test
// REQUIRES: objc_interop

// This is an interpreter test that cannot use swift-darwin-postprocess.py to
// work around the DYLD_LIBRARY_LOAD bug in recent dylds. We need to find an
// alternative workaround for it, such as bumping this test's deployment target.
// REQUIRES: rdar78933143

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
