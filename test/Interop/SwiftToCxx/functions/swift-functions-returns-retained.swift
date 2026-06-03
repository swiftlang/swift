// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Functions -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -I %S/Inputs/ -typecheck -verify -emit-clang-header-path %t/functions.h -disable-availability-checking
// RUN: %FileCheck %s < %t/functions.h

// REQUIRES: objc_interop

// Check if the following annotations are attached to the generated thunk
// signatures, based on the return type:
// - NS_RETURNS_RETAINED for ObjC classes (NSString, etc.)
// - CF_RETURNS_RETAINED for CF types (CFString, etc.)
// - SWIFT_RETURNS_RETAINED for foreign reference types

import Foundation
import CPointerTypes

// CHECK: #if !defined(NS_RETURNS_RETAINED)
// CHECK: #if !defined(CF_RETURNS_RETAINED)
// CHECK: #if !defined(SWIFT_RETURNS_RETAINED)

public final class SwiftClass {
  public init() {}
}


// Properties and methods (declarations)

public final class Foo {
  public var nameCF: CFString { "hello" as NSString }
  public var nameNS: NSString { "hello" as NSString }

  public func getCFString() -> CFString { "hello" as CFString }
  public func getNSString() -> NSString { "hello" as NSString }

  public func getFRT(_ x: FRT) -> FRT { x }

  public func getSwiftClass() -> SwiftClass { SwiftClass() }
}

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nonnull getNameCF()
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nonnull getNameNS()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nonnull getCFString()
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nonnull getNSString()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK FRT *_Nonnull getFRT(
// CHECK-SAME: SWIFT_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK SwiftClass getSwiftClass(
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: ;


// Pass through

public func passThroughCFString(_ x: CFString) -> CFString {
  return x
}

public func passThroughFRT(_ x: FRT) -> FRT {
  return x
}

public func passThroughNSString(_ x: NSString) -> NSString {
  return x
}

public func passThroughSwiftClass(_ x: SwiftClass) -> SwiftClass {
  return x
}

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nonnull passThroughCFString(
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK FRT *_Nonnull passThroughFRT(
// CHECK-SAME: SWIFT_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nonnull passThroughNSString(
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK SwiftClass passThroughSwiftClass(
// CHECK-SAME: SWIFT_WARN_UNUSED_RESULT
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {


// Return

public func returnAnyObject() -> AnyObject {
  fatalError()
}

public func returnCFString() -> CFString {
  return "hello" as CFString
}

public func returnCFStringOpt() -> CFString? {
  return nil
}

public func returnFRT(_ x: FRT) -> FRT {
  return x
}

public func returnFRTOpt(_ x: FRT) -> FRT? {
  return nil
}

public func returnNSString() -> NSString {
  return "hello" as NSString
}

public func returnNSStringOpt() -> NSString? {
  return nil
}

public func returnSwiftClass() -> SwiftClass {
  return SwiftClass()
}

// CHECK: SWIFT_INLINE_THUNK id _Nonnull returnAnyObject()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nonnull returnCFString()
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nullable returnCFStringOpt()
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK FRT *_Nonnull returnFRT(
// CHECK-SAME: SWIFT_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK FRT *_Nullable returnFRTOpt(
// CHECK-SAME: SWIFT_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nonnull returnNSString()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nullable returnNSStringOpt()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK SwiftClass returnSwiftClass()
// CHECK-SAME: SWIFT_WARN_UNUSED_RESULT
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {


// Take

public func takeNSString(_ x: NSString) {}
public func takeFRT(_ x: FRT) {}
public func takeCFString(_ x: NSString) {}
public func takeSwiftClass(_ x: SwiftClass) {}

// CHECK: SWIFT_INLINE_THUNK void takeCFString(
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {

// CHECK: SWIFT_INLINE_THUNK void takeFRT(
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {

// CHECK: SWIFT_INLINE_THUNK void takeNSString(
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {

// CHECK: SWIFT_INLINE_THUNK void takeSwiftClass(
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {


// Properties and methods (definitions)

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nonnull Foo::getNameCF()
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nonnull Foo::getNameNS()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK CFStringRef _Nonnull Foo::getCFString()
// CHECK-SAME: CF_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK NSString *_Nonnull Foo::getNSString()
// CHECK-SAME: NS_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK FRT *_Nonnull Foo::getFRT(
// CHECK-SAME: SWIFT_RETURNS_RETAINED

// CHECK: SWIFT_INLINE_THUNK SwiftClass Foo::getSwiftClass(
// CHECK-NOT: RETURNS_RETAINED
// CHECK-SAME: {
