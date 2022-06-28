// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import CoreFoundation
import Foundation

class SwiftClass { }

func genericCast<T>(_ x: AnyObject, _: T.Type) -> T? {
  return x as? T
}

func genericCastUnconditional<T>(_ x: AnyObject, _: T.Type) -> T {
  return x as! T
}

// Check _cfTypeID() on a Swift class
let nsObject = NSObject()
let swiftObject = SwiftClass()
assert(CFGetTypeID(nsObject) == CFGetTypeID(swiftObject))

// Check CFString <-> AnyObject
func testCFStringAnyObject() {
  // Create a CFString
  let cfStr: CFString
    = CFStringCreateWithCString(nil, "Swift", CFStringBuiltInEncodings.ASCII.rawValue)

  // CHECK: Swift
  print(cfStr)

  // Convert it to AnyObject
  let anyObject: AnyObject = cfStr

  // CHECK: Swift
  print(anyObject)

  // Convert it back to a CFString
  let cfStr2 = anyObject as! CFString

  // CHECK: Swift
  print(cfStr2)

  // Conditional cast through a generic to a CFString
  if let cfStr3 = genericCast(anyObject, CFString.self) {
    // CHECK: Swift
    print(cfStr3)
  } else {
    print("Conditional cast failed")
  }

  // Forced cast through a generic to a CFString
  let cfStr4 = genericCastUnconditional(anyObject, CFString.self)

  // CHECK: Swift
  print(cfStr4)

  // CHECK: done
  print("done")
}
testCFStringAnyObject()


// Check CFString.Type <-> AnyObject.Type
func testCFStringAnyObjectType() {
  let cfStr: CFString
    = CFStringCreateWithCString(nil, "Swift", CFStringBuiltInEncodings.ASCII.rawValue)

  let cfStrType = type(of: cfStr)
  // CHECK: [[STRING_CLASS:(NS|CF).*String]]
  print(cfStrType)

  // Convert to AnyObject.Type
  let anyObjectType: AnyObject.Type = cfStrType
  // CHECK: [[STRING_CLASS]]
  print(anyObjectType)

  // Convert back to CFString.Type
  let cfStrType2 = anyObjectType as! CFString.Type
  // CHECK: [[STRING_CLASS]]
  print(cfStrType2)

  // CHECK: done
  print("done")
}
testCFStringAnyObjectType()

// https://github.com/apple/swift/issues/43022
func testOptionalCFStringToStringAndNSString() {
  let optCFStr: CFString? = "Swift" as CFString

  // CHECK: {{^}}Optional(Swift){{$}}
  print(optCFStr)

  let swiftStr1 = optCFStr as? String
  let swiftStr2 = optCFStr as String?

  let nsStr1 = optCFStr as? NSString
  let nsStr2 = optCFStr as NSString?

  if let unwrapped = swiftStr1 {
    // CHECK-NEXT: {{^}}Swift{{$}}
    print(unwrapped)
  }
  if let unwrapped = swiftStr2 {
    // CHECK-NEXT: {{^}}Swift{{$}}
    print(unwrapped)
  }
  if let unwrapped = nsStr1 {
    // CHECK-NEXT: {{^}}Swift{{$}}
    print(unwrapped)
  }
  if let unwrapped = nsStr2 {
    // CHECK-NEXT: {{^}}Swift{{$}}
    print(unwrapped)
  }

  // CHECK-NEXT: {{^}}done{{$}}
  print("done")
}
testOptionalCFStringToStringAndNSString()
