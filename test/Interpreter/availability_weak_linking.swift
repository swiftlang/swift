// RUN: %empty-directory(%t)
// RUN: cp -R %S/Inputs/FakeUnavailableObjCFramework.framework %t
// RUN: %target-clang -dynamiclib %S/Inputs/FakeUnavailableObjCFramework.m -fmodules -F %t -framework Foundation -o %t/FakeUnavailableObjCFramework.framework/FakeUnavailableObjCFramework
// RUN: %target-codesign %t/FakeUnavailableObjCFramework.framework/FakeUnavailableObjCFramework
// RUN: %target-build-swift-dylib(%t/%target-library-name(FakeUnavailableSwiftDylib)) -emit-module -emit-module-path %t/FakeUnavailableSwiftDylib.swiftmodule %S/Inputs/FakeUnavailableSwiftDylib.swift
// RUN: %target-codesign %t/%target-library-name(FakeUnavailableSwiftDylib)
// RUN: %target-build-swift %t/%target-library-name(FakeUnavailableSwiftDylib) -I %t -F %t %s -o %t/UseWeaklinkedUnavailableObjCFramework
// RUN: %target-build-swift -O %t/%target-library-name(FakeUnavailableSwiftDylib) -I %t -F %t %s -o %t/UseWeaklinkedUnavailableObjCFramework.opt

// These tests emulate deploying back to an older OS where newer APIs are not
// available by linking to an Objective-C framework where APIs have been
// annotated to only be available in the far future (version 1066.0 of all
// platforms) and then moving the framework aside so that it can't be found
// at run time.
// RUN: mv %t/FakeUnavailableObjCFramework.framework %t/FakeUnavailableObjCFramework-MovedAside.framework
// RUN: mv %t/%target-library-name(FakeUnavailableSwiftDylib) %t/%target-library-name(FakeUnavailableSwiftDylib)-MovedAside

// RUN: %target-codesign %t/UseWeaklinkedUnavailableObjCFramework
// RUN: %target-codesign %t/UseWeaklinkedUnavailableObjCFramework.opt
// RUN: %target-run %t/UseWeaklinkedUnavailableObjCFramework | %FileCheck %s
// RUN: %target-run %t/UseWeaklinkedUnavailableObjCFramework.opt | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: executable_test

import StdlibUnittest


import FakeUnavailableObjCFramework
import FakeUnavailableSwiftDylib
import Foundation

// CHECK: Running
print("Running...")

func useUnavailableObjCGlobal() {
  if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
    let g = UnavailableObjCGlobalVariable
    _blackHole(g)
  }
}

useUnavailableObjCGlobal()

@objc
class ClassConformingToUnavailableObjCProtocol : NSObject, UnavailableObjCProtocol {
  func someMethod() {
    print("Executed ClassConformingToUnavailableObjCProtocol.someMethod()")
  }
}

func useClassConformingToUnavailableObjCProtocol() {
  let o = ClassConformingToUnavailableObjCProtocol()
  o.someMethod()

  if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
    let oAsUP: UnavailableObjCProtocol = o as UnavailableObjCProtocol
    oAsUP.someMethod()
  }
}

// CHECK-NEXT: Executed ClassConformingToUnavailableObjCProtocol.someMethod()
useClassConformingToUnavailableObjCProtocol()

@objc
class ClassThatWillBeExtendedToConformToUnavailableObjCProtocol : NSObject {
}

extension ClassThatWillBeExtendedToConformToUnavailableObjCProtocol : UnavailableObjCProtocol {
  func someMethod() {
    print("Executed ClassThatWillBeExtendedToConformToUnavailableObjCProtocol.someMethod()")
  }
}

func useClassThatWillBeExtendedToConformToUnavailableObjCProtocol() {
  let o = ClassThatWillBeExtendedToConformToUnavailableObjCProtocol()
  o.someMethod()

  if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
    let oAsUP: UnavailableObjCProtocol = o as UnavailableObjCProtocol
    oAsUP.someMethod()
  }
}

// CHECK-NEXT: Executed ClassThatWillBeExtendedToConformToUnavailableObjCProtocol.someMethod()
useClassThatWillBeExtendedToConformToUnavailableObjCProtocol()

// We need to gracefully handle ObjC protocols missing availability annotations
// because it is quite common in frameworks. (Historically, for Objective-C,
// missing availability annotations on protocols has not been problematic
// because Objective-C protocol metadata is compiled into any code that
// references it -- it is not weakly linked.)

@objc
class ClassConformingToUnannotatedUnavailableObjCProtocol : NSObject, UnannotatedUnavailableObjCProtocol {
  func someMethod() {
    print("Executed ClassConformingToUnannotatedUnavailableObjCProtocol.someMethod()")
  }
}

func useClassConformingToUnannotatedUnavailableObjCProtocol() {
  let o = ClassConformingToUnannotatedUnavailableObjCProtocol()
  o.someMethod()

  let oAsUP: UnannotatedUnavailableObjCProtocol = (o as AnyObject) as! UnannotatedUnavailableObjCProtocol
  oAsUP.someMethod()
}

// CHECK-NEXT: Executed ClassConformingToUnannotatedUnavailableObjCProtocol.someMethod()
// CHECK-NEXT: Executed ClassConformingToUnannotatedUnavailableObjCProtocol.someMethod()
useClassConformingToUnannotatedUnavailableObjCProtocol()

func printClassMetadataViaGeneric<T>() -> T {
  print("\(T.self)")
  fatalError("This should never be called")
}

func useUnavailableObjCClass() {
  if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
    let o = UnavailableObjCClass()
    o.someMethod()
  }

  for i in 0 ..< getInt(5) {
    if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
      let o: UnavailableObjCClass = printClassMetadataViaGeneric()
      _blackHole(o)
    }
  }

  class SomeClass { }
  let someObject: AnyObject = _opaqueIdentity(SomeClass() as AnyObject)

  for i in 0 ..< getInt(5) {
    if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
      let isUnavailable = someObject is UnavailableObjCClass
      _blackHole(isUnavailable)
    }
  }

  for i in 0 ..< getInt(5) {
    if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
      let asUnavailable = someObject as? UnavailableObjCClass
      _blackHole(asUnavailable)
    }
  }
}

@available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, *)
func wrapUnavailableFunction() {
  someFunction()
}

useUnavailableObjCClass()

// Allow extending a weakly-linked class to conform to a protocol.
protocol SomeSwiftProtocol { }
@available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *)
extension UnavailableObjCClass : SomeSwiftProtocol {
}
@available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *)
extension UnavailableSwiftClass : SomeSwiftProtocol {
}

func checkSwiftProtocolConformance() {
  // Make sure the runtime doesn't crash in the presence of a conformance
  // record for a class that doesn't exist at runtime.
  let x: Any = 42
  _blackHole(x as? SomeSwiftProtocol)
}

checkSwiftProtocolConformance()

class ClassConformingToUnavailableSwiftProtocol : UnavailableSwiftProtocol {
  func someMethod() {
    print("Executed ClassConformingToUnavailableSwiftProtocol.someMethod()")
  }
}

func useClassConformingToUnavailableSwiftProtocol() {
  let o = ClassConformingToUnavailableSwiftProtocol()
  o.someMethod()

  if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
    let oAsUP: UnavailableSwiftProtocol = o as UnavailableSwiftProtocol
    oAsUP.someMethod()
  }
}

// CHECK-NEXT: Executed ClassConformingToUnavailableSwiftProtocol.someMethod()
useClassConformingToUnavailableSwiftProtocol()

class ClassThatWillBeExtendedToConformToUnavailableSwiftProtocol {
}

extension ClassThatWillBeExtendedToConformToUnavailableSwiftProtocol : UnavailableSwiftProtocol {
  func someMethod() {
    print("Executed ClassThatWillBeExtendedToConformToUnavailableSwiftProtocol.someMethod()")
  }
}

func useClassThatWillBeExtendedToConformToUnavailableSwiftProtocol() {
  let o = ClassThatWillBeExtendedToConformToUnavailableSwiftProtocol()
  o.someMethod()

  if #available(OSX 1066.0, iOS 1066.0, watchOS 1066.0, tvOS 1066.0, visionOS 1066.0, *) {
    let oAsUP: UnavailableSwiftProtocol = o as UnavailableSwiftProtocol
    oAsUP.someMethod()
  }
}

// CHECK-NEXT: Executed ClassThatWillBeExtendedToConformToUnavailableSwiftProtocol.someMethod()
useClassThatWillBeExtendedToConformToUnavailableSwiftProtocol()

// CHECK-NEXT: Done
print("Done")
