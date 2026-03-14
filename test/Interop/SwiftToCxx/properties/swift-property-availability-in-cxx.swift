// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Methods -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/methods.h
// RUN: %FileCheck %s < %t/methods.h

// RUN: %check-interop-cxx-header-in-clang(%t/methods.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public struct Struct {
  public var field: Int16

  init() {
    field = 0
  }
    
  @available(macOS 11, *)
  public var getterOnly: Int {
    return 0
  }

  @available(*, unavailable, message: "stuff happened")
  public static var staticUnavailableProp: Int {
    return 0
  }
}

// CHECK:  SWIFT_INLINE_THUNK swift::Int getGetterOnly() const SWIFT_SYMBOL("s:7Methods6StructV10getterOnlySivp") SWIFT_AVAILABILITY(macos,introduced=11);
// CHECK: static SWIFT_INLINE_THUNK swift::Int getStaticUnavailableProp() SWIFT_SYMBOL("s:7Methods6StructV21staticUnavailablePropSivpZ") SWIFT_UNAVAILABLE_MSG("stuff happened");

// CHECK: SWIFT_INLINE_THUNK swift::Int Struct::getGetterOnly() const SWIFT_AVAILABILITY(macos,introduced=11) {

// CHECK: SWIFT_INLINE_THUNK swift::Int Struct::getStaticUnavailableProp() SWIFT_UNAVAILABLE_MSG("stuff happened") {
