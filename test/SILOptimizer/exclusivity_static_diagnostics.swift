// RUN: %target-swift-frontend -enforce-exclusivity=checked -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts<T>(_ p1: inout T, _ p2: inout T) { }

func simpleInoutDiagnostic() {
  var i = 7

  // FIXME: This diagnostic should be removed if static enforcement is
  // turned on by default.
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-error@+2{{overlapping accesses to 'i', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&i, &i)
}

func inoutOnInoutParameter(p: inout Int) {
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-error@+2{{overlapping accesses to 'p', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&p, &p)
}

func swapNoSuppression(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  // expected-error@+2{{overlapping accesses to 'a', but modification requires exclusive access; consider calling MutableCollection.swapAt(_:_:)}}
  // expected-note@+1{{conflicting access is here}}
  swap(&a[i], &a[j])
}

class SomeClass { }

struct StructWithMutatingMethodThatTakesSelfInout {
  var f = SomeClass()
  mutating func mutate(_ other: inout StructWithMutatingMethodThatTakesSelfInout) { }
  mutating func mutate(_ other: inout SomeClass) { }

  mutating func callMutatingMethodThatTakesSelfInout() {
    // expected-error@+4{{inout arguments are not allowed to alias each other}}
    // expected-note@+3{{previous aliasing argument}}
    // expected-error@+2{{overlapping accesses to 'self', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    mutate(&self)
  }

  mutating func callMutatingMethodThatTakesSelfStoredPropInout() {
    // expected-error@+2{{overlapping accesses to 'self', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    mutate(&self.f)
  }
}

var globalStruct1 = StructWithMutatingMethodThatTakesSelfInout()
func callMutatingMethodThatTakesGlobalStoredPropInout() {
  // expected-error@+2{{overlapping accesses to 'globalStruct1', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  globalStruct1.mutate(&globalStruct1.f)
}

class ClassWithFinalStoredProp {
  final var s1: StructWithMutatingMethodThatTakesSelfInout = StructWithMutatingMethodThatTakesSelfInout()
  final var s2: StructWithMutatingMethodThatTakesSelfInout = StructWithMutatingMethodThatTakesSelfInout()

  func callMutatingMethodThatTakesClassStoredPropInout() {
    s1.mutate(&s2.f) // no-warning

    // expected-error@+2{{overlapping accesses to 's1', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    s1.mutate(&s1.f)

    let local1 = self

    // expected-error@+2{{overlapping accesses to 's1', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    local1.s1.mutate(&local1.s1.f)
  }
}

func violationWithGenericType<T>(_ p: T) {
  var local = p
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-error@+2{{overlapping accesses to 'local', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&local, &local)
}


// Helper.
struct StructWithTwoStoredProp {
  var f1: Int = 7
  var f2: Int = 8
}

// Take an unsafe pointer to a stored property while accessing another stored property.
func violationWithUnsafePointer(_ s: inout StructWithTwoStoredProp) {
  // FIXME: This needs to be statically enforced.
  withUnsafePointer(to: &s.f1) { (ptr) in
    _ = s.f1
  }
  // FIXME: We may want to allow this case for known-layout stored properties.
  withUnsafePointer(to: &s.f1) { (ptr) in
    _ = s.f2
  }
}
// Tests for Fix-Its to replace swap(&collection[a], &collection[b]) with
// collection.swapAt(a, b)

struct StructWithField {
  var f = 12
}

struct StructWithFixits {
  var arrayProp: [Int] = [1, 2, 3]
  var dictionaryProp: [Int : Int] = [0 : 10, 1 : 11]

  mutating
  func shouldHaveFixIts<T>(_ i: Int, _ j: Int, _ param: T, _ paramIndex: T.Index) where T : MutableCollection {
    var array1 = [1, 2, 3]
    // expected-error@+2{{overlapping accesses}}{{5-41=array1.swapAt(i + 5, j - 2)}}
    // expected-note@+1{{conflicting access is here}}
    swap(&array1[i + 5], &array1[j - 2])

    // expected-error@+2{{overlapping accesses}}{{5-49=self.arrayProp.swapAt(i, j)}}
    // expected-note@+1{{conflicting access is here}}
    swap(&self.arrayProp[i], &self.arrayProp[j])

    var localOfGenericType = param
    // expected-error@+2{{overlapping accesses}}{{5-75=localOfGenericType.swapAt(paramIndex, paramIndex)}}
    // expected-note@+1{{conflicting access is here}}
    swap(&localOfGenericType[paramIndex], &localOfGenericType[paramIndex])

    // expected-error@+2{{overlapping accesses}}{{5-39=array1.swapAt(i, j)}}
    // expected-note@+1{{conflicting access is here}}
    Swift.swap(&array1[i], &array1[j]) // no-crash
  }

  mutating
  func shouldHaveNoFixIts(_ i: Int, _ j: Int) {
    var s = StructWithField()
    // expected-error@+2{{overlapping accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    swap(&s.f, &s.f)

    var array1 = [1, 2, 3]
    var array2 = [1, 2, 3]

    // Swapping between different arrays should cannot have the
    // Fix-It.
    swap(&array1[i], &array2[j]) // no-warning no-fixit
    swap(&array1[i], &self.arrayProp[j]) // no-warning no-fixit

    // Dictionaries aren't MutableCollections so don't support swapAt().
    // expected-error@+2{{overlapping accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    swap(&dictionaryProp[i], &dictionaryProp[j])

    // We could safely Fix-It this but don't now because the left and
    // right bases are not textually identical.
    // expected-error@+2{{overlapping accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    swap(&self.arrayProp[i], &arrayProp[j])

    // We could safely Fix-It this but we're not that heroic.
    // We don't suppress when swap() is used as a value
    let mySwap: (inout Int, inout Int) -> () = swap

    // expected-error@+2{{overlapping accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    mySwap(&array1[i], &array1[j])

    func myOtherSwap<T>(_ a: inout T, _ b: inout T) {
      swap(&a, &b) // no-warning
    }

    // expected-error@+2{{overlapping accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    mySwap(&array1[i], &array1[j])
  }
}

func takesInoutAndNoEscapeClosure<T>(_ p: inout T, _ c: () -> ()) { }

func callsTakesInoutAndNoEscapeClosure() {
  var local = 5
  takesInoutAndNoEscapeClosure(&local) { // expected-error {{overlapping accesses to 'local', but modification requires exclusive access; consider copying to a local variable}}
    local = 8  // expected-note {{conflicting access is here}}
  }
}

func takesInoutAndNoEscapeClosureThatThrows<T>(_ p: inout T, _ c: () throws -> ()) { }

func callsTakesInoutAndNoEscapeClosureThatThrowsWithNonThrowingClosure() {
  var local = 5
  takesInoutAndNoEscapeClosureThatThrows(&local) { // expected-error {{overlapping accesses to 'local', but modification requires exclusive access; consider copying to a local variable}}
    local = 8  // expected-note {{conflicting access is here}}
  }
}

func takesInoutAndNoEscapeClosureAndThrows<T>(_ p: inout T, _ c: () -> ()) throws { }

func callsTakesInoutAndNoEscapeClosureAndThrows() {
  var local = 5
  try! takesInoutAndNoEscapeClosureAndThrows(&local) { // expected-error {{overlapping accesses to 'local', but modification requires exclusive access; consider copying to a local variable}}
    local = 8  // expected-note {{conflicting access is here}}
  }
}

func takesTwoNoEscapeClosures(_ c1: () -> (), _ c2: () -> ()) { }

func callsTakesTwoNoEscapeClosures() {
  var local = 7
  takesTwoNoEscapeClosures({local = 8}, {local = 9}) // no-error
  _ = local
}

func takesInoutAndEscapingClosure<T>(_ p: inout T, _ c: @escaping () -> ()) { }

func callsTakesInoutAndEscapingClosure() {
  var local = 5
  takesInoutAndEscapingClosure(&local) { // no-error
    local = 8
  }
}

func takesUnsafePointerAndNoEscapeClosure<T>(_ p: UnsafePointer<T>, _ c: () -> ()) { }

func callsTakesUnsafePointerAndNoEscapeClosure() {
  var local = 1
  takesUnsafePointerAndNoEscapeClosure(&local) { // expected-note {{conflicting access is here}}
     local = 2 // expected-error {{overlapping accesses to 'local', but modification requires exclusive access; consider copying to a local variable}}
  }
}

func takesThrowingAutoClosureReturningGeneric<T: Equatable>(_ : @autoclosure () throws -> T) { }
func takesInoutAndClosure<T>(_: inout T, _ : () -> ()) { }

func callsTakesThrowingAutoClosureReturningGeneric() {
  var i = 0
  takesInoutAndClosure(&i) { // expected-error {{overlapping accesses to 'i', but modification requires exclusive access; consider copying to a local variable}}
    takesThrowingAutoClosureReturningGeneric(i) // expected-note {{conflicting access is here}}
  }
}

struct StructWithMutatingMethodThatTakesAutoclosure {
  var f = 2
  mutating func takesAutoclosure(_ p: @autoclosure () throws -> ()) rethrows { }
}

func conflictOnSubPathInNoEscapeAutoclosure() {
  var s = StructWithMutatingMethodThatTakesAutoclosure()
  s.takesAutoclosure(s.f = 2)
  // expected-error@-1 {{overlapping accesses to 's', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2 {{conflicting access is here}}
}

func conflictOnWholeInNoEscapeAutoclosure() {
  var s = StructWithMutatingMethodThatTakesAutoclosure()
  takesInoutAndNoEscapeClosure(&s.f) {
    // expected-error@-1 {{overlapping accesses to 's.f', but modification requires exclusive access; consider copying to a local variable}}
    s = StructWithMutatingMethodThatTakesAutoclosure()
    // expected-note@-1 {{conflicting access is here}}
  }
}

func inoutSeparateStructStoredProperties() {
  var s = StructWithTwoStoredProp()
  takesTwoInouts(&s.f1, &s.f2) // no-error
}

func inoutSameStoredProperty() {
  var s = StructWithTwoStoredProp()
  takesTwoInouts(&s.f1, &s.f1)
  // expected-error@-1{{overlapping accesses to 's.f1', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

func inoutSeparateTupleElements() {
  var t = (1, 4)
  takesTwoInouts(&t.0, &t.1) // no-error
}

func inoutSameTupleElement() {
  var t = (1, 4)
  takesTwoInouts(&t.0, &t.0)
  // expected-error@-1{{overlapping accesses to 't.0', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

func inoutSameTupleNamedElement() {
  var t = (name1: 1, name2: 4)
  takesTwoInouts(&t.name2, &t.name2)
  // expected-error@-1{{overlapping accesses to 't.name2', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

func inoutSamePropertyInSameTuple() {
  var t = (name1: 1, name2: StructWithTwoStoredProp())
  takesTwoInouts(&t.name2.f1, &t.name2.f1)
  // expected-error@-1{{overlapping accesses to 't.name2.f1', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

// Noescape closures and separate stored structs

func callsTakesInoutAndNoEscapeClosureNoWarningOnSeparateStored() {
  var local = StructWithTwoStoredProp()
  takesInoutAndNoEscapeClosure(&local.f1) {
    local.f2 = 8 // no-error
  }
}

func callsTakesInoutAndNoEscapeClosureWarningOnSameStoredProp() {
  var local = StructWithTwoStoredProp()
  takesInoutAndNoEscapeClosure(&local.f1) { // expected-error {{overlapping accesses to 'local.f1', but modification requires exclusive access; consider copying to a local variable}}
    local.f1 = 8 // expected-note {{conflicting access is here}}
  }
}

func callsTakesInoutAndNoEscapeClosureWarningOnAggregateAndStoredProp() {
  var local = StructWithTwoStoredProp()
  takesInoutAndNoEscapeClosure(&local) { // expected-error {{overlapping accesses to 'local', but modification requires exclusive access; consider copying to a local variable}}
    local.f1 = 8 // expected-note {{conflicting access is here}}
  }
}

func callsTakesInoutAndNoEscapeClosureWarningOnStoredPropAndAggregate() {
  var local = StructWithTwoStoredProp()
  takesInoutAndNoEscapeClosure(&local.f1) { // expected-error {{overlapping accesses to 'local.f1', but modification requires exclusive access; consider copying to a local variable}}
    local = StructWithTwoStoredProp() // expected-note {{conflicting access is here}}
  }
}

func callsTakesInoutAndNoEscapeClosureWarningOnStoredPropAndBothPropertyAndAggregate() {
  var local = StructWithTwoStoredProp()
  takesInoutAndNoEscapeClosure(&local.f1) { // expected-error {{overlapping accesses to 'local.f1', but modification requires exclusive access; consider copying to a local variable}}
    local.f1 = 8
    // We want the diagnostic on the access for the aggregate and not the projection.
    local = StructWithTwoStoredProp() // expected-note {{conflicting access is here}}
  }
}

func callsTakesInoutAndNoEscapeClosureWarningOnStoredPropAndBothAggregateAndProperty() {
  var local = StructWithTwoStoredProp()
  takesInoutAndNoEscapeClosure(&local.f1) { // expected-error {{overlapping accesses to 'local.f1', but modification requires exclusive access; consider copying to a local variable}}
    // We want the diagnostic on the access for the aggregate and not the projection.
    local = StructWithTwoStoredProp() // expected-note {{conflicting access is here}}
    local.f1 = 8
  }
}


struct MyStruct<T> {
  var prop = 7
  mutating func inoutBoundGenericStruct() {
    takesTwoInouts(&prop, &prop)
    // expected-error@-1{{overlapping accesses to 'self.prop', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@-2{{conflicting access is here}}
  }
}
