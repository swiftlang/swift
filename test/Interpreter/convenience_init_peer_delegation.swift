// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift %s -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// RUN: %empty-directory(%t)
//
// RUN: sed -e 's/required//g' < %s > %t/without_required.swift
// RUN: %target-build-swift %t/without_required.swift -o %t/without_required
// RUN: %target-codesign %t/without_required
// RUN: %target-run %t/without_required | %FileCheck %s

// RUN: %empty-directory(%t)
//
// RUN: %target-build-swift %s -o %t/main -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// RUN: %empty-directory(%t)
//
// RUN: sed -e 's/required//g' < %s > %t/without_required.swift
// RUN: %target-build-swift %t/without_required.swift -o %t/without_required -swift-version 5
// RUN: %target-codesign %t/without_required
// RUN: %target-run %t/without_required | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop
// REQUIRES: foundation
// XFAIL: CPU=arm64e

// Because of the use of 'sed' in this test.
// REQUIRES: shell

import Darwin
import Foundation

class Base {
  init(swift: ()) {
    print("\(#function) \(type(of: self))")
  }
  @objc(initAsObjC) required init(objc: ()) {
    print("\(#function) \(type(of: self))")
  }

  convenience init(swiftToSwift: ()) {
    print("\(#function) \(type(of: self))")
    self.init(swift: ())
  }
  @objc convenience required init(objcToSwift: ()) {
    print("\(#function) \(type(of: self))")
    self.init(swift: ())
  }
  convenience init(swiftToObjC: ()) {
    print("\(#function) \(type(of: self))")
    self.init(objc: ())
  }
  @objc convenience required init(objcToObjC: ()) {
    print("\(#function) \(type(of: self))")
    self.init(objc: ())
  }

  convenience init(swiftToSwiftConvenience: ()) {
    print("\(#function) \(type(of: self))")
    self.init(swiftToSwift: ())
  }
  @objc convenience required init(objcToSwiftConvenience: ()) {
    print("\(#function) \(type(of: self))")
    self.init(swiftToSwift: ())
  }
  convenience init(swiftToObjCConvenience: ()) {
    print("\(#function) \(type(of: self))")
    self.init(objcToObjC: ())
  }
  @objc convenience required init(objcToObjCConvenience: ()) {
    print("\(#function) \(type(of: self))")
    self.init(objcToObjC: ())
  }
}

class Sub: Base {}

@objc protocol ForceObjCDispatch {
  @objc(initAsObjC) init(objc: ())
  init(objcToSwift: ())
  init(objcToObjC: ())
  init(objcToSwiftConvenience: ())
  init(objcToObjCConvenience: ())
}

// Replace swift_allocObject so that we can keep track of what gets allocated.
var baseCounter = 0
var subCounter = 0

typealias AllocObjectType =
    @convention(c) (UnsafeRawPointer, Int, Int) -> UnsafeMutableRawPointer
let allocObjectImpl =
  dlsym(UnsafeMutableRawPointer(bitPattern: -1), "_swift_allocObject")
    .assumingMemoryBound(to: AllocObjectType.self)

/// Like `ObjectIdentifier.init(Any.Type)`, but with a pointer as the
/// destination type.
func asUnsafeRawPointer(_ someClass: AnyObject.Type) -> UnsafeRawPointer {
  let opaque = Unmanaged.passUnretained(someClass as AnyObject).toOpaque()
  return UnsafeRawPointer(opaque)
}

let originalAllocObject = allocObjectImpl.pointee
allocObjectImpl.pointee = {
  switch $0 {
  case asUnsafeRawPointer(Base.self):
    baseCounter += 1
  case asUnsafeRawPointer(Sub.self):
    subCounter += 1
  default:
    break
  }

  return originalAllocObject($0, $1, $2)
}

/// Checks that `op` performs `base` allocations of Base and `sub` allocations
/// of Sub.
@inline(never)
func check(base: Int = 0, sub: Int = 0,
           file: StaticString = #file, line: UInt = #line,
           op: () -> AnyObject) {
  baseCounter = 0
  subCounter = 0
  _ = op()
  precondition(baseCounter == base,
               "expected \(base) Base instances, got \(baseCounter)",
               file: file, line: line)
  precondition(subCounter == sub,
               "expected \(sub) Sub instances, got \(subCounter)",
               file: file, line: line)
}


// Specialization of the 'check' function may result in the closure being deleted, which breaks the test. Disabling
// optimization of this calling function prevents closure specialization.
@_optimize(none)
func main() {
  // CHECK: START
  print("START")
   
  // Check that this whole setup works.
  // CHECK-NEXT: init(swift:) Base
  check(base: 1) { Base(swift: ()) }
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 1) { Sub(swift: ()) }
  // CHECK-NEXT: init(objc:) Base
  check(base: 1) { Base(objc: ()) }
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { Sub(objc: ()) }
   
  // CHECK-NEXT: init(swiftToSwift:) Sub
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 1) { Sub(swiftToSwift: ()) }
  // CHECK-NEXT: init(objcToSwift:) Sub
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 2) { Sub(objcToSwift: ()) }
  // CHECK-NEXT: init(swiftToObjC:) Sub
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { Sub(swiftToObjC: ()) }
  // CHECK-NEXT: init(objcToObjC:) Sub
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { Sub(objcToObjC: ()) }
   
  // CHECK-NEXT: init(swiftToSwiftConvenience:) Sub
  // CHECK-NEXT: init(swiftToSwift:) Sub
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 1) { Sub(swiftToSwiftConvenience: ()) }
  // CHECK-NEXT: init(objcToSwiftConvenience:) Sub
  // CHECK-NEXT: init(swiftToSwift:) Sub
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 2) { Sub(objcToSwiftConvenience: ()) }
  // CHECK-NEXT: init(swiftToObjCConvenience:) Sub
  // CHECK-NEXT: init(objcToObjC:) Sub
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { Sub(swiftToObjCConvenience: ()) }
  // CHECK-NEXT: init(objcToObjCConvenience:) Sub
  // CHECK-NEXT: init(objcToObjC:) Sub
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { Sub(objcToObjCConvenience: ()) }
   
  // Force ObjC dispatch without conforming Sub or Base to the protocol,
  // because it's possible that `required` perturbs things and we want to test
  // both ways.
  let SubAsObjC = unsafeBitCast(Sub.self as AnyObject,
                                to: ForceObjCDispatch.Type.self)
   
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { SubAsObjC.init(objc: ()) }
  // CHECK-NEXT: init(objcToSwift:) Sub
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 2) { SubAsObjC.init(objcToSwift: ()) }
  // CHECK-NEXT: init(objcToObjC:) Sub
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { SubAsObjC.init(objcToObjC: ()) }
  // CHECK-NEXT: init(objcToSwiftConvenience:) Sub
  // CHECK-NEXT: init(swiftToSwift:) Sub
  // CHECK-NEXT: init(swift:) Sub
  check(sub: 2) { SubAsObjC.init(objcToSwiftConvenience: ()) }
  // CHECK-NEXT: init(objcToObjCConvenience:) Sub
  // CHECK-NEXT: init(objcToObjC:) Sub
  // CHECK-NEXT: init(objc:) Sub
  check(sub: 1) { SubAsObjC.init(objcToObjCConvenience: ()) }

  // CHECK-NEXT: END
  print("END")
}

main()

