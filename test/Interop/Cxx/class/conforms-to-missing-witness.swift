// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -module-name a -cxx-interoperability-mode=default -I %t%{fs-sep}include %t%{fs-sep}a.swift -verify -verify-additional-file %t%{fs-sep}include%{fs-sep}header.h -o /dev/null

//--- include/module.modulemap
module cxx {
  header "header.h"
  export *
}

//--- include/header.h
struct MissingMethod {
} __attribute__((swift_attr("conforms_to:a.P")));

struct HasMethod {
  void f() const {}
} __attribute__((swift_attr("conforms_to:a.P")));

struct NearMiss {
  void f() {} // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
} __attribute__((swift_attr("conforms_to:a.P")));

struct Base {
} __attribute__((swift_attr("conforms_to:a.P")));

struct Derived : Base {
};

// Each derived type gets its own conformance, with its own witnesses.
struct DerivedAddsWitness : Base {
  void f() const {}
};

struct InheritedMissing {
  void f() const {}
} __attribute__((swift_attr("conforms_to:a.R")));

struct Ambiguous {
  void h(char16_t) const {} // expected-note 2 {{candidate exactly matches}}
  void h(unsigned short) const {} // expected-note 2 {{candidate exactly matches}}
} __attribute__((swift_attr("conforms_to:a.S")));

struct DerivedAmbiguous : Ambiguous {
};

struct DerivedResolvesAmbiguity : Ambiguous {
  void h(unsigned short) const {}
};

struct HasAssoc {
  using Element = int;
  int get() const { return 0; }
} __attribute__((swift_attr("conforms_to:a.T")));

// Associated types are not inferred from the value witnesses.
struct InferredAssoc {
  int get() const { return 0; }
} __attribute__((swift_attr("conforms_to:a.T")));

struct WrongAssoc {
  using Element = int;
  float get() const { return 0; } // expected-note {{candidate has non-matching type '() -> CFloat' (aka '() -> Float')}}
} __attribute__((swift_attr("conforms_to:a.T")));

struct NoCopyOrMove {
  NoCopyOrMove() {}
  NoCopyOrMove(const NoCopyOrMove &) = delete;
  NoCopyOrMove(NoCopyOrMove &&) = delete;
};

struct NoDestructor {
  NoDestructor() {}
  ~NoDestructor() = delete;
};

// The witnesses exist, but the types they use cannot be imported.
struct ReturnsNoCopyOrMove {
  using Element = NoCopyOrMove;
  NoCopyOrMove get() const;
} __attribute__((swift_attr("conforms_to:a.T")));

struct ReturnsNoDestructor {
  using Element = NoDestructor;
  NoDestructor get() const;
} __attribute__((swift_attr("conforms_to:a.T")));

//--- a.swift
import cxx

// expected-error@+3 {{type 'MissingMethod' does not conform to protocol 'P'}}
// expected-error@+2 {{type 'NearMiss' does not conform to protocol 'P'}}
// expected-error@+1 {{type 'Derived' does not conform to protocol 'P'}}
public protocol P {
  func f() // expected-note 3 {{protocol requires function 'f()' with type '() -> ()'}}
}

// expected-error@+1 {{type 'InheritedMissing' does not conform to protocol 'Q'}}
public protocol Q {
  func g() // expected-note {{protocol requires function 'g()' with type '() -> ()'}}
}

public protocol R: Q {
  func f()
}

// expected-error@+2 {{type 'Ambiguous' does not conform to protocol 'S'}}
// expected-error@+1 {{type 'DerivedAmbiguous' does not conform to protocol 'S'}}
public protocol S {
  func h(_: UInt16) // expected-note 2 {{multiple matching functions named 'h' with type '(UInt16) -> ()'}}
}

// A missing associated type is diagnosed once, by the importer.
// expected-error@+5 {{type 'InferredAssoc' does not conform to protocol 'T'}}
// expected-error@+4 {{type 'ReturnsNoCopyOrMove' does not conform to protocol 'T'}}
// expected-error@+3 {{type 'ReturnsNoDestructor' does not conform to protocol 'T'}}
// expected-error@+2 {{type 'WrongAssoc' does not conform to protocol 'T'}}
// expected-note@+1 3 {{protocol requires nested type 'Element'}}
public protocol T {
  associatedtype Element
  func get() -> Element // expected-note {{protocol requires function 'get()' with type '() -> WrongAssoc.Element' (aka '() -> Int32')}}
}

func take(_ x: some P) {}
func take(_ x: some R) {}
func take(_ x: some S) {}
func take(_ x: some T) {}

func test() {
  take(MissingMethod())
  take(HasMethod())
  take(NearMiss())
  take(Derived())
  take(DerivedAddsWitness())
  take(InheritedMissing())
  take(Ambiguous())
  take(DerivedAmbiguous())
  take(DerivedResolvesAmbiguity())
  take(HasAssoc())
  take(InferredAssoc())
  take(WrongAssoc())
  take(ReturnsNoCopyOrMove())
  take(ReturnsNoDestructor())
}
