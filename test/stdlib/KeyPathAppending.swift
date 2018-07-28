// RUN: %target-typecheck-verify-swift

// Check that all combinations of key paths produce the expected result type
// and choose the expected overloads.

#if BUILDING_OUTSIDE_STDLIB
import Swift
#endif

func expect<T>(_: inout T, is: T.Type) {}

func wellTypedAppends<T, U, V>(readOnlyLeft: KeyPath<T, U>,
                               writableLeft: WritableKeyPath<T, U>,
                               referenceLeft: ReferenceWritableKeyPath<T, U>,
                               readOnlyRight: KeyPath<U, V>,
                               writableRight: WritableKeyPath<U, V>,
                               referenceRight: ReferenceWritableKeyPath<U, V>){
  var a = readOnlyLeft.appending(path: readOnlyRight)
  expect(&a, is: KeyPath<T, V>.self)

  var b = readOnlyLeft.appending(path: writableRight)
  expect(&b, is: KeyPath<T, V>.self)

  var c = readOnlyLeft.appending(path: referenceRight)
  expect(&c, is: ReferenceWritableKeyPath<T, V>.self)

  var d = writableLeft.appending(path: readOnlyRight)
  expect(&d, is: KeyPath<T, V>.self)

  var e = writableLeft.appending(path: writableRight)
  expect(&e, is: WritableKeyPath<T, V>.self)

  var f = writableLeft.appending(path: referenceRight)
  expect(&f, is: ReferenceWritableKeyPath<T, V>.self)

  var g = referenceLeft.appending(path: readOnlyRight)
  expect(&g, is: KeyPath<T, V>.self)

  var h = referenceLeft.appending(path: writableRight)
  expect(&h, is: ReferenceWritableKeyPath<T, V>.self)

  var i = referenceLeft.appending(path: referenceRight)
  expect(&i, is: ReferenceWritableKeyPath<T, V>.self)
}

func mismatchedAppends<T, U, V>(readOnlyLeft: KeyPath<T, U>,
                                writableLeft: WritableKeyPath<T, U>,
                                referenceLeft: ReferenceWritableKeyPath<T, U>,
                                readOnlyRight: KeyPath<U, V>,
                                writableRight: WritableKeyPath<U, V>,
                                referenceRight: ReferenceWritableKeyPath<U, V>){
  // expected-error@+1{{}}
  _ = readOnlyRight.appending(path: readOnlyLeft)

  // expected-error@+1{{}}
  _ = readOnlyRight.appending(path: writableLeft)

  // expected-error@+1{{}}
  _ = readOnlyRight.appending(path: referenceLeft)

  // expected-error@+1{{}}
  _ = writableRight.appending(path: readOnlyLeft)

  // expected-error@+1{{}}
  _ = writableRight.appending(path: writableLeft)

  // expected-error@+1{{}}
  _ = writableRight.appending(path: referenceLeft)

  // expected-error@+1{{}}
  _ = referenceRight.appending(path: readOnlyLeft)

  // expected-error@+1{{}}
  _ = referenceRight.appending(path: writableLeft)

  // expected-error@+1{{}}
  _ = referenceRight.appending(path: referenceLeft)
}

func partialAppends<T, U, V>(partial: PartialKeyPath<T>,
                             concrete: KeyPath<U, V>,
                             reference: ReferenceWritableKeyPath<U, V>,
                             any: AnyKeyPath) {
  var a = any.appending(path: any)
  expect(&a, is: Optional<AnyKeyPath>.self)

  var b = any.appending(path: partial)
  expect(&b, is: Optional<AnyKeyPath>.self)

  var c = any.appending(path: concrete)
  expect(&c, is: Optional<AnyKeyPath>.self)

  var d = any.appending(path: reference)
  expect(&d, is: Optional<AnyKeyPath>.self)


  var e = partial.appending(path: any)
  expect(&e, is: Optional<PartialKeyPath<T>>.self)

  var f = partial.appending(path: partial)
  expect(&f, is: Optional<PartialKeyPath<T>>.self)

  var g = partial.appending(path: concrete)
  expect(&g, is: Optional<KeyPath<T, V>>.self)

  var h = partial.appending(path: reference)
  expect(&h, is: Optional<ReferenceWritableKeyPath<T, V>>.self)


  /* TODO
  var i = concrete.appending(path: any)
  expect(&i, is: Optional<PartialKeyPath<U>>.self)

  var j = concrete.appending(path: partial)
  expect(&j, is: Optional<PartialKeyPath<U>>.self)


  var m = reference.appending(path: any)
  expect(&m, is: Optional<PartialKeyPath<U>>.self)

  var n = reference.appending(path: partial)
  expect(&n, is: Optional<PartialKeyPath<U>>.self)
   */
}
