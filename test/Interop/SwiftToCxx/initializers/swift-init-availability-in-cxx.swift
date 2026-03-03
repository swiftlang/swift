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
  public init(_ field: Int16) {
    self.field = field
  }
}

// CHECK: static SWIFT_INLINE_THUNK Struct init(int16_t field) SWIFT_SYMBOL("s:7Methods6StructVyACs5Int16Vcfc") SWIFT_AVAILABILITY(macos,introduced=11);

// CHECK: SWIFT_INLINE_THUNK Struct Struct::init(int16_t field) SWIFT_AVAILABILITY(macos,introduced=11) {
