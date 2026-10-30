// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/local.swift -module-name CImports -typecheck -verify -cxx-interoperability-mode=default -import-objc-header %t/bridge.h -emit-clang-header-path %t/local.h
// RUN: %FileCheck %s < %t/local.h
// RUN: %check-interop-cxx-header-in-clang(%t/local.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-c-header-in-clang(%t/local.h)

// The C section also imports the bridging header when a @c declaration refers
// to an imported C type. Local C++ traits must already be available there.

// CHECK: class SWIFT_SYMBOL("{{.*}}") Item;
// CHECK: inline const constexpr bool isUsableInGenericContext<CImports::Item> = true;
// CHECK: #include "{{.*}}bridge.h"
// CHECK: SWIFT_EXTERN ImportedToken passToken(ImportedToken value)
// CHECK: class SWIFT_SYMBOL("{{.*}}") Item final {

//--- local.swift
public struct Item {
  public var value: Int
}

@c(passToken)
public func passToken(_ value: ImportedToken) -> ImportedToken { value }

//--- bridge.h
#pragma once

typedef struct ImportedToken {
  int value;
} ImportedToken;

// This part of the bridge is consumed from the generated header, after Swift
// has finished importing the C declarations above.
#if defined(__cplusplus) && defined(CIMPORTS_SWIFT_H)
namespace CImports {
class Item;
}
static_assert(swift::isUsableInGenericContext<CImports::Item>, "");
#endif
