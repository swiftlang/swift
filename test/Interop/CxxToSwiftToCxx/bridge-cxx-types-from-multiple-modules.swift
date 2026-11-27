// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/first.swift -module-name First -emit-module -emit-module-path %t/First.swiftmodule -emit-clang-header-path %t/First.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public
// RUN: %target-swift-frontend %t/second.swift -module-name Second -typecheck -verify -emit-clang-header-path %t/Second.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public
// RUN: %target-swift-frontend %t/third.swift -module-name Third -typecheck -verify -emit-clang-header-path %t/Third.h -I %t -cxx-interoperability-mode=default -clang-header-expose-decls=all-public -clang-header-expose-module First=First.h

// RUN: %FileCheck %s --check-prefixes=CHECK,FIRST --input-file %t/First.h
// RUN: %FileCheck %s --check-prefixes=CHECK,SECOND --input-file %t/Second.h
// RUN: %FileCheck %s --check-prefix=THIRD --input-file %t/Third.h

// RUN: %check-interop-cxx-header-in-clang(-I %t %t/first-and-second.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(-I %t %t/third-only.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)

// Every header that uses an imported type in a generic context prints the
// type's generic traits behind the same guard.

// CHECK:      // Type metadata accessor for Cxx$Struct
// CHECK:      namespace swift SWIFT_PRIVATE_ATTR {
// CHECK-NEXT: #ifndef SWIFT_GENERIC_TRAITS_sSo10Cxx$StructVMa
// CHECK-NEXT: #define SWIFT_GENERIC_TRAITS_sSo10Cxx$StructVMa

// CHECK:      // Type metadata accessor for CxxEnum
// CHECK:      namespace swift SWIFT_PRIVATE_ATTR {
// CHECK-NEXT: #ifndef SWIFT_GENERIC_TRAITS_sSo7CxxEnumVMa
// CHECK-NEXT: #define SWIFT_GENERIC_TRAITS_sSo7CxxEnumVMa

// CHECK:      // Type metadata accessor for CxxStruct
// CHECK:      namespace swift SWIFT_PRIVATE_ATTR {
// CHECK-NEXT: #ifndef SWIFT_GENERIC_TRAITS_sSo9CxxStructVMa
// CHECK-NEXT: #define SWIFT_GENERIC_TRAITS_sSo9CxxStructVMa
// CHECK-NEXT: #pragma clang diagnostic push
// CHECK-NEXT: #pragma clang diagnostic ignored "-Wc++17-extensions"
// CHECK-NEXT: template<>
// CHECK-NEXT: inline const constexpr bool isUsableInGenericContext<CxxStruct> = true;
// CHECK-NEXT: template<>
// CHECK-NEXT: struct TypeMetadataTrait<CxxStruct> {
// CHECK-NEXT:   static SWIFT_INLINE_PRIVATE_HELPER void * _Nonnull getTypeMetadata() {
// CHECK-NEXT:     return {{First|Second}}::_impl::$sSo9CxxStructVMa(0)._0;
// CHECK-NEXT:   }
// CHECK-NEXT: };
// CHECK-NEXT: namespace _impl{
// CHECK-NEXT: template<>
// CHECK-NEXT: inline const constexpr bool isSwiftBridgedCxxRecord<CxxStruct> = true;
// CHECK-NEXT: } // namespace
// CHECK-NEXT: #pragma clang diagnostic pop
// CHECK-NEXT: #endif // SWIFT_GENERIC_TRAITS_sSo9CxxStructVMa
// CHECK-NEXT: } // namespace swift

// The guard comes from the canonical type, so different typedefs of one
// template specialization share it.

// FIRST:      // Type metadata accessor for CxxTemplateInt
// SECOND:     // Type metadata accessor for CxxTemplateIntAlias
// CHECK:      namespace swift SWIFT_PRIVATE_ATTR {
// CHECK-NEXT: #ifndef SWIFT_GENERIC_TRAITS_sSo0023CxxTemplateCInt_omFDedbVMa
// CHECK-NEXT: #define SWIFT_GENERIC_TRAITS_sSo0023CxxTemplateCInt_omFDedbVMa

// A header that includes another module's header still guards its traits.

// THIRD:      #include <First.h>
// THIRD:      // Type metadata accessor for CxxStruct
// THIRD:      namespace swift SWIFT_PRIVATE_ATTR {
// THIRD-NEXT: #ifndef SWIFT_GENERIC_TRAITS_sSo9CxxStructVMa

//--- header.h

struct CxxStruct {
  int x;
};

#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wdollar-in-identifier-extension"
struct Cxx$Struct {
  int x;
};
#pragma clang diagnostic pop

enum class CxxEnum { a, b };

template <class T>
struct CxxTemplate {
  T x;
};

using CxxTemplateInt = CxxTemplate<int>;
using CxxTemplateIntAlias = CxxTemplate<int>;

//--- module.modulemap
module CxxTest {
  header "header.h"
  requires cplusplus
}

//--- first.swift
public import CxxTest

public struct Box {
  public var value: Int32
  public init(value: Int32) { self.value = value }
}

public func firstDollar(_ x: Cxx$Struct?) -> Int32 {
  return x?.x ?? -1
}

public func firstEnum(_ x: CxxEnum?) -> Bool {
  return x == .b
}

public func firstStruct(_ x: CxxStruct?) -> Int32 {
  return x?.x ?? -1
}

public func firstTemplate(_ x: CxxTemplateInt?) -> Int32 {
  return x?.x ?? -1
}

//--- second.swift
public import CxxTest

public func secondDollar(_ x: Cxx$Struct?) -> Int32 {
  return x?.x ?? -1
}

public func secondEnum(_ x: CxxEnum?) -> Bool {
  return x == .b
}

public func secondStruct(_ xs: [CxxStruct]) -> Int32 {
  return xs.reduce(0) { $0 + $1.x }
}

public func secondTemplate(_ x: CxxTemplateIntAlias?) -> Int32 {
  return x?.x ?? -1
}

//--- third.swift
public import CxxTest
public import First

public func thirdBox(_ x: CxxStruct?) -> Box {
  return Box(value: firstStruct(x) + 1)
}

//--- first-and-second.h
#include "header.h"
#include "First.h"
#include "Second.h"

//--- third-only.h
#include "header.h"
#include "Third.h"
