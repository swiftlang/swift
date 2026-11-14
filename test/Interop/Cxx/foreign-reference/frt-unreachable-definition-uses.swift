// An FRT by inheritance whose definition is unreachable (NS::Anchor below) is
// imported as unavailable, instead of being rejected when it is imported.
// Any Swift code that could copy or destroy a value containing it is rejected.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// DEFINE: %{frontend} = %target-swift-frontend -I %t%{fs-sep}Inputs -cxx-interoperability-mode=default -Xcc -std=c++20 -target %target-swift-5.8-abi-triple -parse-as-library -module-name main

// Using an unrelated operator doesn't trip over Anchor, even though resolving
// it imports a C++ operator== whose parameter types contain Anchor.
// RUN: %{frontend} -emit-ir %t/int-equals.swift -o %t/int-equals.ll

// RUN: %{frontend} -typecheck -verify %t%{fs-sep}uses.swift -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}Hidden.h

// Records that C++ copies and destroys are opaque to Swift, so copying them is
// fine, since Swift does not need to retain or release them.
// RUN: %{frontend} -emit-ir %t/copied-by-cxx.swift -o /dev/null

//--- Inputs/module.modulemap
module Lib {
  explicit module Base { header "Base.h" export * }
  // Explicit and not imported by Swift, so Anchor's definition is unreachable.
  explicit module Hidden { header "Hidden.h" export * }
  explicit module Visible { header "Visible.h" export * }
}

//--- Inputs/Base.h
#pragma once

namespace NS {
template <typename T>
class __attribute__((swift_attr("import_reference")))
      __attribute__((swift_attr("retain:.ref")))
      __attribute__((swift_attr("release:.deref")))
      __attribute__((swift_attr("returned_as_unretained_by_default")))
      RefCountedBase {
public:
  void ref() const {}
  void deref() const {}
};
} // namespace NS

//--- Inputs/Hidden.h
#pragma once
#include "Base.h"

namespace NS {
// An FRT by inheritance from RefCountedBase.
// The note is for the two uses in uses.swift that name Anchor.
// expected-note@+1 2 {{'Anchor' has been explicitly marked unavailable here}}
class Anchor : public RefCountedBase<Anchor> {};
} // namespace NS

//--- Inputs/Visible.h
#pragma once
#include "Base.h"

namespace NS {
class Anchor; // Only forward-declared.

Anchor *makeAnchor();

struct Registry {
  Anchor *get() const;
};

struct Handle {
  const Anchor *anchor;
};
struct Key {
  Handle handle;
};
inline bool operator==(const Key &, const Key &) { return true; }
Key makeKey();

struct HandleBase {
  Anchor *anchor;
};
struct DerivedHandle : HandleBase {};
DerivedHandle makeDerivedHandle();

template <typename T>
struct Box {
  T *ptr;
};
using AnchorBox = Box<Anchor>;
AnchorBox makeAnchorBox();

// Non-trivial in C++: Swift copies it with the copy constructor and destroys
// it with the destructor, never touching the field itself.
struct CopiedByCxx {
  const Anchor *anchor;
  CopiedByCxx(const CopiedByCxx &other) : anchor(other.anchor) {}
  ~CopiedByCxx() {}
};
CopiedByCxx makeCopiedByCxx();

// Non-trivial copy constructor, but a trivial (implicit) destructor. Swift
// copies it with the copy constructor, but destroys it field by field.
struct CopiedByCxxTrivialDtor {
  const Anchor *anchor;
  CopiedByCxxTrivialDtor(const CopiedByCxxTrivialDtor &other)
      : anchor(other.anchor) {}
};
CopiedByCxxTrivialDtor makeCopiedByCxxTrivialDtor();

// Trivial in C++, with a private FRT field.
class Widget {
  const Anchor *impl;

public:
  int size() const { return 0; }
};
Widget makeWidget();
} // namespace NS

//--- int-equals.swift
import Lib.Visible

public func probe(a: Int, b: Int) -> Bool {
  a == b
}

//--- uses.swift
import Lib.Visible

// MARK: Naming Anchor

public func namesAnchor(_ a: NS.Anchor) {} // expected-error {{foreign reference type 'Anchor' has unreachable definition}}
                                           // expected-error@-1 {{contains foreign reference type 'Anchor'}}

public func namesOptionalAnchor() {
  let _: NS.Anchor? = nil // expected-error {{foreign reference type 'Anchor' has unreachable definition}}
                          // expected-error@-1 {{contains foreign reference type 'Anchor'}}
}

// MARK: Expressions

public func returnAnchor() {
  let a = NS.makeAnchor() // expected-error {{cannot be used because it contains foreign reference type 'Anchor', whose definition is not reachable}}
  let b = a
  _ = (a, b)
}

public func methodAnchor() {
  let r = NS.Registry()
  let a = r.get() // expected-error {{contains foreign reference type 'Anchor'}}
  _ = a
}

public func copyKey() {
  let k = NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
  let k2 = k
  _ = (k, k2)
}

public func destroyKey() {
  _ = NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
}

public func fieldRead() {
  _ = NS.makeKey().handle.anchor // expected-error {{contains foreign reference type 'Anchor'}}
}

public func inheritedField() {
  _ = NS.makeDerivedHandle().anchor // expected-error {{contains foreign reference type 'Anchor'}}
}

// Fields of base classes count as fields of the derived record.
public func derivedCopy() {
  let d = NS.makeDerivedHandle() // expected-error {{contains foreign reference type 'Anchor'}}
  let d2 = d
  _ = (d, d2)
}

public func cxxOperator() -> Bool {
  NS.makeKey() == NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
}

@inline(never)
func identity<T>(_ x: T) -> T { x }

public func generic() {
  let k = identity(NS.makeKey()) // expected-error {{contains foreign reference type 'Anchor'}}
  _ = k
}

public func existential() {
  let x: Any = NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
  _ = x
}

public func array() {
  let keys = [NS.makeKey(), NS.makeKey()] // expected-error {{contains foreign reference type 'Anchor'}}
  _ = keys
}

public func closure() {
  let k = NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
  let f = { _ = k }
  f()
}

// A closure shares its errors with the enclosing function.
public func onlyInClosure() {
  let f = { _ = NS.makeKey() } // expected-error {{contains foreign reference type 'Anchor'}}
  f()
}

// A local function shares its errors with the enclosing function.
public func localFunction() {
  _ = NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
  func local(_ k: NS.Key) {}
}

public func template() {
  let b = NS.makeAnchorBox() // expected-error {{contains foreign reference type 'Anchor'}}
  let b2 = b
  _ = (b, b2)
}

public func widget() {
  let w = NS.makeWidget() // expected-error {{contains foreign reference type 'Anchor'}}
  let w2 = w
  _ = (w, w2)
}

public func copiedByCxxTrivialDtor() {
  let c = NS.makeCopiedByCxxTrivialDtor() // expected-error {{contains foreign reference type 'Anchor'}}
  let c2 = c
  _ = (c, c2)
}

public func metatype() {
  _ = NS.Key.self // expected-error {{contains foreign reference type 'Anchor'}}
}

public func keyPath() {
  _ = \NS.Key.handle // expected-error {{contains foreign reference type 'Anchor'}}
}

public func functionValue() {
  let f = NS.makeKey // expected-error {{contains foreign reference type 'Anchor'}}
  _ = f
}

// MARK: Declarations

struct Wrapper {
  var key: NS.Key // expected-error {{type 'NS.Key' cannot be used because it contains foreign reference type 'Anchor', whose definition is not reachable}}
}

public func swiftStruct() {
  let w = Wrapper(key: NS.makeKey()) // expected-error {{contains foreign reference type 'Anchor'}}
  let w2 = w
  _ = (w, w2)
}

struct WrapperWithInitializer {
  var key = NS.makeKey() // expected-error {{contains foreign reference type 'Anchor'}}
}

enum HoldsKey {
  case key(NS.Key) // expected-error {{contains foreign reference type 'Anchor'}}
}

typealias KeyAlias = NS.Key // expected-error {{contains foreign reference type 'Anchor'}}

extension NS.Key { // expected-error {{contains foreign reference type 'Anchor'}}
  func method() {}
}

func takesWrapper(_ w: Wrapper) {} // expected-error {{type 'Wrapper' cannot be used because it contains foreign reference type 'Anchor'}}

public func takesWidget(_ w: NS.Widget) {} // expected-error {{type 'NS.Widget' cannot be used because it contains foreign reference type 'Anchor'}}

// The declaration's error covers its body.
public func returnsKey() -> NS.Key { // expected-error {{contains foreign reference type 'Anchor'}}
  NS.makeKey()
}

public func forIn(keys: [NS.Key]) { // expected-error {{type '[NS.Key]' cannot be used because it contains foreign reference type 'Anchor'}}
  for k in keys {
    _ = k
  }
}

public func ifCase(maybe: NS.Key?) { // expected-error {{contains foreign reference type 'Anchor'}}
  if case let .some(k) = maybe {
    _ = k
  }
}

// A loop whose only exposing types are in the desugared preamble and body.
public func forInLocal() {
  for k in [NS.makeKey()] { // expected-error {{contains foreign reference type 'Anchor'}}
    _ = k
  }
}

// MARK: Allowed

public func intEquals(a: Int, b: Int) -> Bool {
  a == b
}

public func registry() {
  let r = NS.Registry()
  _ = r
}

// Copied and destroyed by C++.
public func copiedByCxx() {
  let c = NS.makeCopiedByCxx()
  let c2 = c
  _ = (c, c2)
}

//--- copied-by-cxx.swift
import Lib.Visible

public func test() {
  let c = NS.makeCopiedByCxx()
  let c2 = c
  _ = (c, c2)
}
