// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend %t/support.swift -module-name Support -emit-module -emit-module-path %t/Support.swiftmodule -clang-header-expose-decls=all-public -emit-clang-header-path %t/support.h
// RUN: %target-swift-frontend %t/local.swift -module-name Local -I %t -typecheck -verify -clang-header-expose-decls=has-expose-attr -clang-header-expose-module Support=bridge.h -emit-clang-header-path %t/local.h
// RUN: %FileCheck %s --implicit-check-not=Hidden --implicit-check-not=Internal --implicit-check-not='isUsableInGenericContext<Local::Unexposed>' < %t/local.h
// RUN: %check-interop-cxx-header-in-clang(-I %t %t/local.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-swift-frontend %t/other.swift -module-name Other -typecheck -clang-header-expose-decls=all-public -emit-clang-header-path %t/other.h
// RUN: %check-interop-cxx-header-in-clang(-I %t %t/consumer.h -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %check-interop-cxx-header-in-clang(-I %t %t/consumer.h -DREVERSE_INCLUDES -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)
// RUN: %target-interop-build-clangxx -std=c++17 -fsyntax-only -x c++-header %t/consumer.h -I %t -fmodules -fcxx-modules -fmodules-cache-path=%t/module-cache
// RUN: %target-interop-build-clangxx -std=c++20 -fsyntax-only -x c++-header %t/consumer.h -I %t -fmodules -fcxx-modules -fmodules-cache-path=%t/module-cache

// The imported bridge names constrained generic types before the local type
// definitions. A forward declaration alone does not make a Swift type usable as
// a generic argument. Its eligibility specialization must precede the import,
// while metadata and complete definitions can remain after it.

// CHECK: class SWIFT_SYMBOL("{{.*}}") RenamedItem;
// CHECK: inline const constexpr bool isUsableInGenericContext<Local::RenamedItem> = true;
// CHECK: #include <bridge.h>
// CHECK: class SWIFT_SYMBOL("{{.*}}") RenamedItem final {
// CHECK: struct TypeMetadataTrait<Local::RenamedItem>
// CHECK: class Unexposed { } SWIFT_UNAVAILABLE_MSG

//--- support.swift
public struct ImportedToken {
  public var value: Int
}

// Supply real Array and Optional declarations independently of the local header.
public func array(_ value: [Int]) -> [Int] { value }
public func optional(_ value: Int?) -> Int? { value }

//--- local.swift
import Support

@_expose(Cxx, "RenamedItem")
public struct Item {
  public var value: Int
}

@_expose(Cxx)
public enum Kind {
  case first, second
}

@_expose(Cxx)
public struct Holder {
  public var value: Int

  @_expose(Cxx)
  public struct Nested {
    public var value: Int
  }
}

@_expose(Cxx)
public struct Box<T> {
  public var value: T
}

@_expose(!Cxx)
public struct Hidden { public var value: Int }
struct Internal { var value: Int }
public struct Unexposed { public var value: Int }

@_expose(Cxx)
public func importedToken(_ value: ImportedToken) -> ImportedToken { value }

@_expose(Cxx)
public func array(_ value: [Item]) -> [Item] { value }

@_expose(Cxx)
public func optional(_ value: Item?) -> Item? { value }

//--- bridge.h
#pragma once
#include "support.h"

// Bridge headers may forward declare local Swift names, but cannot define their
// Swift generic eligibility specializations themselves.
namespace Local {
class RenamedItem;
class Kind;
}

template<class Signature> struct Callback;
template<class Result> struct Promise;

using ArrayCallback = Callback<void(const swift::Array<Local::RenamedItem>&)>;
using EnumArrayCallback = Callback<swift::Array<Local::Kind>()>;
using OptionalCallback = Callback<void(const swift::Optional<Local::RenamedItem>&)>;
using ArrayPromise = Promise<swift::Array<Local::RenamedItem>>;
using BoxArray = swift::Array<Local::Box<Local::RenamedItem>>;
using NestedOptional = swift::Optional<Local::__HolderNested::Nested>;

// Before C++20 the container's static_assert is checked only when the class is
// instantiated. Check the early traits directly in those language modes too.
static_assert(swift::isUsableInGenericContext<Local::RenamedItem>, "");
static_assert(swift::isUsableInGenericContext<Local::Kind>, "");
static_assert(swift::isUsableInGenericContext<Local::Box<Local::RenamedItem>>, "");
static_assert(swift::isUsableInGenericContext<Local::__HolderNested::Nested>, "");

template<class T, class = void> struct IsComplete {
  static constexpr bool value = false;
};
template<class T> struct IsComplete<T, decltype(void(sizeof(T)))> {
  static constexpr bool value = true;
};
static_assert(!IsComplete<Local::RenamedItem>::value, "local definition must follow this import");
static_assert(!IsComplete<Local::Kind>::value, "local definition must follow this import");

//--- other.swift
public struct Item {
  public var value: Int
}
public func array(_ value: [Item]) -> [Item] { value }
public func optional(_ value: Item?) -> Item? { value }

//--- consumer.h
#ifdef REVERSE_INCLUDES
#include "other.h"
#include "local.h"
#else
#include "local.h"
#include "other.h"
#endif
#include "support.h"

static_assert(swift::isUsableInGenericContext<Local::RenamedItem>, "");
static_assert(swift::isUsableInGenericContext<Other::Item>, "");
using LocalArray = swift::Array<Local::RenamedItem>;
using OtherOptional = swift::Optional<Other::Item>;

//--- module.modulemap
module LocalHeader {
  header "local.h"
  requires cplusplus
  export *
}
module OtherHeader {
  header "other.h"
  requires cplusplus
  export *
}
