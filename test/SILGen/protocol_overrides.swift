// RUN: %target-swift-emit-silgen -module-name main %s | %FileCheck %s

// rdar://47470420

/*****************************************************************************/
/* Methods *******************************************************************/
/*****************************************************************************/

protocol HasMutatingMethod {
  mutating func foo()
}

protocol HasMutatingMethodClone: HasMutatingMethod {
  mutating func foo()
}
extension HasMutatingMethodClone {
// CHECK-LABEL: sil hidden [ossa] @$s4main22HasMutatingMethodClonePAAE10performFooyyF :
// CHECK:         witness_method $Self, #HasMutatingMethod.foo :
  mutating func performFoo() {
    foo()
  }
}

protocol HasNonMutatingMethod: HasMutatingMethod {
  func foo()
}
extension HasNonMutatingMethod {
// CHECK-LABEL: sil hidden [ossa] @$s4main20HasNonMutatingMethodPAAE10performFooyyF :
// CHECK:         witness_method $Self, #HasNonMutatingMethod.foo :
  func performFoo() {
    foo()
  }
}

/*****************************************************************************/
/* Getters *******************************************************************/
/*****************************************************************************/

protocol HasMutatingGetter {
  var foo: Int { mutating get }
}

protocol HasMutatingGetterClone: HasMutatingGetter {
  var foo: Int { mutating get }
}
extension HasMutatingGetterClone {
// CHECK-LABEL: sil hidden [ossa] @$s4main22HasMutatingGetterClonePAAE11readFromFooSiyF :
// CHECK:         witness_method $Self, #HasMutatingGetter.foo!getter :
  mutating func readFromFoo() -> Int {
    return foo
  }
}

protocol HasNonMutatingGetter: HasMutatingGetter {
  var foo: Int { get }
}
extension HasNonMutatingGetter {
// CHECK-LABEL: sil hidden [ossa] @$s4main20HasNonMutatingGetterPAAE11readFromFooSiyF :
// CHECK:         witness_method $Self, #HasNonMutatingGetter.foo!getter :
  func readFromFoo() -> Int {
    return foo
  }
}

/*****************************************************************************/
/* Setters *******************************************************************/
/*****************************************************************************/

protocol HasMutatingSetter {
  var foo: Int { get set }
}

protocol HasMutatingSetterClone: HasMutatingSetter {
  var foo: Int { get set }
}
extension HasMutatingSetterClone {
// CHECK-LABEL: sil hidden [ossa] @$s4main22HasMutatingSetterClonePAAE11readFromFooSiyF :
// CHECK:         witness_method $Self, #HasMutatingSetter.foo!getter :
  func readFromFoo() -> Int {
    return foo
  }

// CHECK-LABEL: sil hidden [ossa] @$s4main22HasMutatingSetterClonePAAE10writeToFooyySiF :
// CHECK:         witness_method $Self, #HasMutatingSetter.foo!setter :
  mutating func writeToFoo(_ x: Int) {
    foo = x
  }
}

protocol HasNonMutatingSetter: HasMutatingSetter {
  var foo: Int { get nonmutating set }
}
extension HasNonMutatingSetter {
//   It's unfortunate that this uses a new witness table entry,
//   but we can live with it.
// CHECK-LABEL: sil hidden [ossa] @$s4main20HasNonMutatingSetterPAAE11readFromFooSiyF :
// CHECK:         witness_method $Self, #HasNonMutatingSetter.foo!getter :
  func readFromFoo() -> Int {
    return foo
  }

// CHECK-LABEL: sil hidden [ossa] @$s4main20HasNonMutatingSetterPAAE10writeToFooyySiF :
// CHECK:         witness_method $Self, #HasNonMutatingSetter.foo!setter :
  func writeToFoo(_ x: Int) {
    foo = x
  }
}
