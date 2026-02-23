// RUN: %empty-directory(%t)
// RUN: split-file %s %t
//
// Validate usability of type.h in C++ with clang
// RUN: %target-clang -c -o /dev/null -Xclang -verify -I %t/Inputs %t/ok.cpp
// RUN: %target-clang -c -o /dev/null -Xclang -verify=cxx-instantiation -I %t/Inputs %t/err.cpp
//
// Compare usability of type.h in Swift with swift-frontend
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}type.h \
// RUN:   %t%{fs-sep}ok.swift
// RUN: %target-swift-frontend -typecheck -verify -cxx-interoperability-mode=default \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -I %t%{fs-sep}Inputs -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}type.h \
// RUN:   %t%{fs-sep}err.swift -verify-additional-prefix swift-
//
// Check module interface of type.h
// RUN: %target-swift-ide-test -print-module -source-filename=x \
// RUN:   -cxx-interoperability-mode=default -I %t/Inputs \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN:   -module-to-print=Type | %FileCheck %s

// REQUIRES: swift_feature_ImportCxxMembersLazily

//--- Inputs/module.modulemap
module Type {
    header "type.h"
    requires cplusplus
}

//--- Inputs/type.h
#pragma once

// cxx-instantiation-note@+3 {{template is declared here}}
// cxx-instantiation-note@+2 {{template is declared here}}
// cxx-instantiation-note@+1 {{template is declared here}}
template <typename T> struct NeedsBool;

template <> struct NeedsBool<bool> {};
// CHECK:      struct NeedsBool<CBool> {
// CHECK-NEXT:   init()
// CHECK-NEXT: }

struct Foo { using foo = int; };
struct Bar {};
struct Baz {};

// expected-swift-note@+1 {{'NeedsFoo<Bar>' requested here}}
template <typename T> struct NeedsFoo {
  typename T::foo x;
  // cxx-instantiation-error@-1 {{no type named 'foo' in 'Bar'}}
  // expected-swift-error@-2 {{no type named 'foo' in 'Bar'}}
  // cxx-instantiation-error@-3 {{no type named 'foo' in 'Baz'}}
};
// CHECK:      struct NeedsFoo<Foo> {
// CHECK-NEXT:   init(x: Foo.foo)
// CHECK-NEXT:   init()
// CHECK-NEXT:   var x: Foo.foo
// CHECK-NEXT: }

// expected-swift-note@+2 {{declared here}}
// expected-swift-note@+1 {{declared here}}
struct UseMe {
  using GoodBool = NeedsBool<bool>;
  using GoodFoo = NeedsFoo<Foo>;

  using BadBool = NeedsBool<int>;
  using BadFoo = NeedsFoo<Bar>;
  using BadFoo2 = NeedsFoo<Baz>;
};
// CHECK:      struct UseMe {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias GoodBool = NeedsBool<CBool>
// CHECK-NEXT:   typealias GoodFoo = NeedsFoo<Foo>
// CHECK-NEXT: }

// Test using shadow decls with type aliases
// expected-swift-note@+2 {{declared here}}
// expected-swift-note@+1 {{declared here}}
struct Derived : UseMe {
  using UseMe::GoodBool;
  using UseMe::GoodFoo;
  using UseMe::BadBool;
  using UseMe::BadFoo2;
};
// CHECK:      struct Derived {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias GoodBool = UseMe.GoodBool
// CHECK-NEXT:   typealias GoodFoo = UseMe.GoodFoo
// CHECK-NEXT: }

// expected-swift-note@+1 {{declared here}}
struct FwdContainer {
  struct Forward;
  using ForwardAlias = Forward;

  // cxx-instantiation-note@+1 {{forward declaration of}}
  struct NeverDefined;
  using UndefinedAlias = NeverDefined;
};
struct FwdContainer::Forward {
  int value;
};
// CHECK:      struct FwdContainer {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias ForwardAlias = FwdContainer.Forward
// CHECK-NEXT:   struct Forward {
// CHECK-NEXT:     init(value: Int32)
// CHECK-NEXT:     init()
// CHECK-NEXT:     var value: Int32
// CHECK-NEXT:   }
// CHECK-NEXT: }

// expected-swift-note@+2 {{declared here}}
template <typename T, typename U>
struct TemplateContainer {
  using GoodAlias = NeedsBool<T>;
  using BadAlias = NeedsBool<U>;
};

using TemplateContainerInst = TemplateContainer<bool, int>;
// CHECK:      struct TemplateContainer<CBool, CInt> {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias GoodAlias = NeedsBool<CBool>
// CHECK-NEXT: }
// CHECK-NEXT: typealias TemplateContainerInst = TemplateContainer<CBool, CInt>

// cxx-instantiation-note@+2 {{forward declaration of}}
// expected-swift-note@+1 {{forward declaration of}}
struct IncompleteType;

// Test type alias to template with invalid instantiation
// expected-swift-note@+2 {{requested here}}
template <typename T>
struct RequiresComplete {
  T value;
  // cxx-instantiation-error@-1 {{field has incomplete type 'IncompleteType'}}
  // expected-swift-error@-2 {{field has incomplete type 'IncompleteType'}}
};

// expected-swift-note@+1 {{declared here}}
struct UsesInvalidTemplate {
  using BadTemplate = RequiresComplete<IncompleteType>;
};
// CHECK:      struct UsesInvalidTemplate {
// CHECK-NEXT:   init()
// CHECK-NEXT: }

// Test type alias to abstract class
// expected-swift-note@+1 {{'init()' has been explicitly marked unavailable here}}
struct Abstract {
  // cxx-instantiation-note@+1 {{unimplemented pure virtual method}}
  virtual void pureVirtual() = 0;
};
// CHECK:      struct Abstract {
// CHECK-NEXT:   @available(*, unavailable, message: "constructors of abstract C++ classes are unavailable in Swift")
// CHECK-NEXT:   init()
// CHECK-NEXT:   @available(*, unavailable, message: "virtual function is not available in Swift because it is pure")
// CHECK-NEXT:   mutating func pureVirtual()
// CHECK-NEXT: }

struct UsesAbstract {
  using AbstractAlias = Abstract;
};
// CHECK:      struct UsesAbstract {
// CHECK-NEXT:   init()
// CHECK-NEXT:   typealias AbstractAlias = Abstract
// CHECK-NEXT: }

//--- ok.cpp
// expected-no-diagnostics
#include <type.h>

void ok(void) {
  UseMe um;
  UseMe::GoodBool gb;
  UseMe::GoodFoo gf;

  Derived dum;
  Derived::GoodBool dgb;
  Derived::GoodFoo dgf;

  FwdContainer fc;
  FwdContainer::ForwardAlias fa;

  TemplateContainerInst tci;
  TemplateContainerInst::GoodAlias tga;

  UsesInvalidTemplate uit;
  UsesAbstract ua;
}

//--- err.cpp
#include <type.h>

void err(void) {
  UseMe um;
  UseMe::GoodBool gb;
  UseMe::GoodFoo gf;
  UseMe::BadBool bb; // cxx-instantiation-error {{implicit instantiation of undefined template}}
  UseMe::BadFoo bf;  // cxx-instantiation-note {{requested here}}

  Derived dum;
  Derived::GoodBool dgb;
  Derived::GoodFoo dgf;
  Derived::BadBool dbb; // cxx-instantiation-error {{implicit instantiation of undefined template}}
  Derived::BadFoo2 dbf; // cxx-instantiation-note {{requested here}}

  FwdContainer fc;
  FwdContainer::ForwardAlias fa;
  FwdContainer::UndefinedAlias fu; // cxx-instantiation-error {{variable has incomplete type}}

  TemplateContainerInst tci;
  TemplateContainerInst::GoodAlias tga;
  TemplateContainerInst::BadAlias tba; // cxx-instantiation-error {{implicit instantiation of undefined template}}

  UsesInvalidTemplate uit;
  UsesInvalidTemplate::BadTemplate ubt; // cxx-instantiation-note {{requested here}}

  UsesAbstract ua;
  UsesAbstract::AbstractAlias uaa; // cxx-instantiation-error {{is an abstract class}}
}

//--- ok.swift
import Type

func ok() {
  let _: UseMe = UseMe()
  let _: UseMe.GoodBool = .init()
  let _: UseMe.GoodFoo = .init()

  let _: Derived = .init()
  let _: Derived.GoodBool = .init()
  let _: Derived.GoodFoo = .init()

  let _: FwdContainer = .init()
  let _: FwdContainer.ForwardAlias = .init()

  let _: TemplateContainerInst = .init()
  let _: TemplateContainerInst.GoodAlias = .init()

  let _: UsesInvalidTemplate = .init()
  let _: UsesAbstract = .init()
}

//--- err.swift
import Type

func err() {
  let _: UseMe = UseMe()
  let _: UseMe.GoodBool = .init()
  let _: UseMe.GoodFoo = .init()
  let _: UseMe.BadBool = .init() // expected-swift-error {{not a member type of struct}}
  let _: UseMe.BadFoo = .init()  // expected-swift-error {{not a member type of struct}}

  let _: Derived = .init()
  let _: Derived.GoodBool = .init()
  let _: Derived.GoodFoo = .init()
  let _: Derived.BadBool = .init() // expected-swift-error {{not a member type of struct}}
  let _: Derived.BadFoo = .init()  // expected-swift-error {{not a member type of struct}}

  let _: FwdContainer = .init()
  let _: FwdContainer.ForwardAlias = .init()
  let _: FwdContainer.UndefinedAlias = .init() // expected-swift-error {{not a member type of struct}}

  let _: TemplateContainerInst = .init()
  let _: TemplateContainerInst.GoodAlias = .init()
  let _: TemplateContainerInst.BadAlias = .init() // expected-swift-error {{not a member type of struct}}

  let _: UsesInvalidTemplate = .init()
  let _: UsesInvalidTemplate.BadTemplate = .init() // expected-swift-error {{not a member type of struct}}
  let _: UsesAbstract = .init()
  let _: UsesAbstract.AbstractAlias = .init() // expected-swift-error {{constructors of abstract C++ classes are unavailable}}
}
