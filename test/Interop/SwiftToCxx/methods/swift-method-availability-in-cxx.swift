// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Methods -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/methods.h
// RUN: %FileCheck %s < %t/methods.h

// RUN: %check-interop-cxx-header-in-clang(%t/methods.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct Struct {
  var field: Int16

  init() {
    field = 0
  }
    
  @available(macOS 11, *)
  public func method() {}

  @available(macOS 11, *)
  public static func staticMethod() {}

  @available(*, unavailable, message: "stuff happened")
  public func unavailableMethod() {}

  @available(macOS 11, *)
  public subscript (_ x: Int) -> Int {
    return 0
  }
}

// CHECK: SWIFT_INLINE_THUNK void method() const SWIFT_SYMBOL("s:7Methods6StructV6methodyyF") SWIFT_AVAILABILITY(macos,introduced=11);
// CHECK: static SWIFT_INLINE_THUNK void staticMethod() SWIFT_SYMBOL("s:7Methods6StructV12staticMethodyyFZ") SWIFT_AVAILABILITY(macos,introduced=11);
// CHECK: SWIFT_INLINE_THUNK void unavailableMethod() const SWIFT_SYMBOL("s:7Methods6StructV17unavailableMethodyyF") SWIFT_UNAVAILABLE_MSG("stuff happened");
// CHECK: SWIFT_INLINE_THUNK swift::Int operator [](swift::Int x) const SWIFT_SYMBOL("s:7Methods6StructVyS2icig") SWIFT_AVAILABILITY(macos,introduced=11);

// CHECK: SWIFT_INLINE_THUNK void Struct::method() const SWIFT_AVAILABILITY(macos,introduced=11) {

// CHECK: SWIFT_INLINE_THUNK void Struct::staticMethod() SWIFT_AVAILABILITY(macos,introduced=11) {

// CHECK: SWIFT_INLINE_THUNK void Struct::unavailableMethod() const SWIFT_UNAVAILABLE_MSG("stuff happened") {

// CHECK: SWIFT_INLINE_THUNK swift::Int Struct::operator [](swift::Int x) const SWIFT_SYMBOL("s:7Methods6StructVyS2icig") SWIFT_AVAILABILITY(macos,introduced=11)
