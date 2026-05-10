
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Class -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/class.h
// RUN: %FileCheck %s < %t/class.h

// RUN: %check-interop-cxx-header-in-clang(%t/class.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public class FileUtilities {
  public static let shared = FileUtilities()
  public let field = 42;
}

// CHECK: SWIFT_INLINE_THUNK FileUtilities FileUtilities::getShared() {
