// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

// RUN: %check-interop-cxx-header-in-clang(%t/enums.h -Wno-unused-private-field -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

public enum E {
  case foo([E?])
}

// CHECK: class SWIFT_SYMBOL("s:5Enums1EO") E final {
// CHECK: SWIFT_INLINE_THUNK swift::Array<swift::Optional<E>> getFoo() const;
