// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-ir -primary-file %t/valid.swift %t/definitions.swift > /dev/null
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix main- -primary-file %t/main.swift %t/definitions.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-additional-prefix other- -parse-as-library -primary-file %t/other.swift %t/definitions.swift

//--- definitions.swift
@attached(peer)
macro SomeAttr() = #externalMacro(module: "", type: "") // expected-note {{declared here}}
// expected-main-note@-1 2{{declared here}}

@propertyWrapper
struct SomeAttr<T> {
  var wrappedValue: T
  init(wrappedValue: T) { self.wrappedValue = wrappedValue }
  init(projectedValue: Self) { self = projectedValue}
  var projectedValue: Self { self }
}

struct Err: Error {}

//--- valid.swift
func foo() throws {
  // Make sure we prefer the property wrapper over the macro here.
  @SomeAttr var x = 0
  let _: Int = x
  let _: SomeAttr<Int> = _x
  let _: SomeAttr<Int> = $x

  _ = {
    @SomeAttr var y = 0
    let _: Int = y
    let _: SomeAttr<Int> = _y
    let _: SomeAttr<Int> = $y
  }

  func bar(@SomeAttr x: Int) {
    let _: Int = x
    let _: SomeAttr<Int> = _x
    let _: SomeAttr<Int> = $x
  }

  bar($x: SomeAttr(wrappedValue: 0))

  func baz(_ fn: (SomeAttr<Int>) -> Void) {}
  baz { x in }
  baz { $x in }

  _ = if .random() {
    @SomeAttr var z = 0
    let _: SomeAttr<Int> = _z
    let _: SomeAttr<Int> = $z
    throw Err()
  } else {
    0
  }
}

//--- main.swift
struct S1 {
  var x = if .random() {
    @SomeAttr var y = 0
    let _: SomeAttr<Int> = _y
    let _: SomeAttr<Int> = $y
    throw Err() // expected-error {{errors cannot be thrown out of a property initializer}}
  } else {
    0
  }
}

// Here we prefer the macro.
struct S2 {
  @SomeAttr var x = 0 // expected-error {{could not be found for macro}}
}

func bar() {
  @SomeAttr func baz() {} // expected-error {{could not be found for macro}}
}

@SomeAttr var x = 0 // expected-error {{could not be found for macro}}

_ = { @SomeAttr<Int> in }
// expected-error@-1 {{attribute @SomeAttr<Int> is not supported on a closure}}
// expected-warning@-2 {{cannot specialize a non-generic external macro 'SomeAttr()'}}

//--- other.swift

// Also check for a library file
@SomeAttr var x = 0 // expected-error {{could not be found for macro}}
