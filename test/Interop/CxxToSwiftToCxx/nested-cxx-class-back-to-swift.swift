// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/use-cxx-types.swift -module-name UseCxxTy -typecheck -verify -emit-clang-header-path %t/UseCxxTy.h -I %t -enable-experimental-cxx-interop -clang-header-expose-decls=all-public
// RUN: %FileCheck %s --input-file %t/UseCxxTy.h
// RUN: cat %t/header.h >> %t/full-header.h
// RUN: cat %t/UseCxxTy.h >> %t/full-header.h
// RUN: %target-interop-build-clangxx -std=c++20 -c -xc++-header %t/full-header.h -o %t/o.o

//--- header.h

struct Cell { class Visitor {}; };

namespace ns {
template <class T> struct Outer { struct Inner { int x; }; };
inline Outer<int>::Inner makeInner() { return {}; }
} // namespace ns

//--- module.modulemap
module CxxTest {
    header "header.h"
    requires cplusplus
}

//--- use-cxx-types.swift
import CxxTest

public extension Cell.Visitor {
    func visit() {}
}

public func f() -> [Cell.Visitor] {
}

// A nested type of a class template specialization cannot be named in Swift.
public struct HasInner {
    public let inner = ns.makeInner()
}

// The type metadata accessor of a nested C++ type is declared and called in
// the _impl namespace of the module.

// CHECK: // Type metadata accessor for Inner
// CHECK-NEXT: SWIFT_EXTERN swift::_impl::MetadataResponseTy [[INNER_MA:\$sSo2nsO.*InnerVMa]](swift::_impl::MetadataRequestTy) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-NEXT: } // end namespace
// CHECK: struct TypeMetadataTrait<ns::Outer<int>::Inner> {
// CHECK-NEXT:   static SWIFT_INLINE_PRIVATE_HELPER void * _Nonnull getTypeMetadata() {
// CHECK-NEXT:     return UseCxxTy::_impl::[[INNER_MA]](0)._0;

// CHECK: SWIFT_INLINE_THUNK ns::Outer<int>::Inner getInner() const noexcept

// CHECK: // Type metadata accessor for Visitor
// CHECK-NEXT: SWIFT_EXTERN swift::_impl::MetadataResponseTy [[VISITOR_MA:\$sSo4CellV7VisitorVMa]](swift::_impl::MetadataRequestTy) SWIFT_NOEXCEPT SWIFT_CALL;
// CHECK-EMPTY:
// CHECK-EMPTY:
// CHECK-NEXT: } // namespace _impl
// CHECK-EMPTY:
// CHECK-NEXT: } // end namespace
// CHECK: struct TypeMetadataTrait<Cell::Visitor> {
// CHECK-NEXT:   static SWIFT_INLINE_PRIVATE_HELPER void * _Nonnull getTypeMetadata() {
// CHECK-NEXT:     return UseCxxTy::_impl::[[VISITOR_MA]](0)._0;
