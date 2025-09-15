// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -typecheck -I %S/Inputs/custom-modules %s -import-underlying-module -verify

import CoreCooling

func testSomeClass(_ sc: SomeClass, osc: SomeClass?) {
  let ao1: Any = sc.methodA(osc)
  _ = ao1
  if sc.methodA(osc) == nil { } // expected-warning {{comparing non-optional value of type 'Any' to 'nil' always returns false}}

  let ao2: Any = sc.methodB(nil)
  _ = ao2
  if sc.methodA(osc) == nil { }// expected-warning {{comparing non-optional value of type 'Any' to 'nil' always returns false}}

  let ao3: Any? = sc.property.flatMap { .some($0) }
  _ = ao3
  let ao3_ok: Any? = sc.property // okay
  _ = ao3_ok

  let ao4: Any = sc.methodD()
  _ = ao4
  if sc.methodD() == nil { } // expected-warning {{comparing non-optional value of type 'Any' to 'nil' always returns false}}

  sc.methodE(sc)
  sc.methodE(osc) // expected-error{{value of optional type 'SomeClass?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}

  sc.methodF(sc, second: sc)
  sc.methodF(osc, second: sc) // expected-error{{value of optional type 'SomeClass?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}
  sc.methodF(sc, second: osc) // expected-error{{value of optional type 'SomeClass?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}

  sc.methodG(sc, second: sc)
  sc.methodG(osc, second: sc) // expected-error{{value of optional type 'SomeClass?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}
  sc.methodG(sc, second: osc) 

  let ci: CInt = 1
  let sc2 = SomeClass(int: ci)
  let sc2a: SomeClass = sc2
  _ = sc2a
  if sc2 == nil { } // expected-warning {{comparing non-optional value of type 'SomeClass' to 'nil' always returns false}}

  let sc3 = SomeClass(double: 1.5)
  if sc3 == nil { } // okay
  let sc3a: SomeClass = sc3 // expected-error{{value of optional type 'SomeClass?' must be unwrapped}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}
  _ = sc3a

  let sc4 = sc.returnMe()
  let sc4a: SomeClass = sc4
  _ = sc4a
  if sc4 == nil { } // expected-warning {{comparing non-optional value of type 'SomeClass' to 'nil' always returns false}}
}

// Nullability with CF types.
func testCF(_ fridge: CCRefrigerator) {
  CCRefrigeratorOpenDoSomething(fridge) // okay
  CCRefrigeratorOpenDoSomething(nil) // expected-error{{'nil' is not compatible with expected argument type 'CCRefrigerator'}}

  CCRefrigeratorOpenMaybeDoSomething(fridge) // okay
  CCRefrigeratorOpenMaybeDoSomething(nil) // okay

  CCRefrigeratorOpenMaybeDoSomething(5) // expected-error{{cannot convert value of type 'Int' to expected argument type 'CCRefrigerator'}}
}
