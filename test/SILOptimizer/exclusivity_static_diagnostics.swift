// RUN: %target-swift-frontend -enforce-exclusivity=checked -swift-version 4 -emit-sil -primary-file %s -o /dev/null -verify

import Swift

func takesTwoInouts<T>(_ p1: inout T, _ p2: inout T) { }

func simpleInoutDiagnostic() {
  var i = 7

  // FIXME: This diagnostic should be removed if static enforcement is
  // turned on by default.
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-error@+2{{simultaneous accesses to var 'i', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&i, &i)
}

func inoutOnInoutParameter(p: inout Int) {
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-error@+2{{simultaneous accesses to parameter 'p', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  takesTwoInouts(&p, &p)
}

func swapNoSuppression(_ i: Int, _ j: Int) {
  var a: [Int] = [1, 2, 3]

  // expected-error@+2{{simultaneous accesses to var 'a', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  swap(&a[i], &a[j]) // no-warning
}

class SomeClass { }

struct StructWithMutatingMethodThatTakesSelfInout {
  var f = SomeClass()
  mutating func mutate(_ other: inout StructWithMutatingMethodThatTakesSelfInout) { }
  mutating func mutate(_ other: inout SomeClass) { }

  mutating func callMutatingMethodThatTakesSelfInout() {
    // expected-error@+4{{inout arguments are not allowed to alias each other}}
    // expected-note@+3{{previous aliasing argument}}
    // expected-error@+2{{simultaneous accesses to parameter 'self', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    mutate(&self)
  }

  mutating func callMutatingMethodThatTakesSelfStoredPropInout() {
    // expected-error@+2{{simultaneous accesses to parameter 'self', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    mutate(&self.f)
  }
}

var globalStruct1 = StructWithMutatingMethodThatTakesSelfInout()
func callMutatingMethodThatTakesGlobalStoredPropInout() {
  // expected-error@+2{{simultaneous accesses to var 'globalStruct1', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@+1{{conflicting access is here}}
  globalStruct1.mutate(&globalStruct1.f)
}

class ClassWithFinalStoredProp {
  final var s1: StructWithMutatingMethodThatTakesSelfInout = StructWithMutatingMethodThatTakesSelfInout()
  final var s2: StructWithMutatingMethodThatTakesSelfInout = StructWithMutatingMethodThatTakesSelfInout()

  func callMutatingMethodThatTakesClassStoredPropInout() {
    s1.mutate(&s2.f) // no-warning

    // expected-error@+2{{simultaneous accesses to var 's1', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    s1.mutate(&s1.f)

    let local1 = self

    // expected-error@+2{{simultaneous accesses to var 's1', but modification requires exclusive access; consider copying to a local variable}}
    // expected-note@+1{{conflicting access is here}}
    local1.s1.mutate(&local1.s1.f)
  }
}

func violationWithGenericType<T>(_ p: T) {
  var local = p
  // expected-error@+4{{inout arguments are not allowed to alias each other}}
  // expected-note@+3{{previous aliasing argument}}
  // expected-error@+2{{simultaneous accesses to var 'local', but modification requires exclusive access; consider copying to a local variable}}
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
    // expected-error@+2{{simultaneous accesses}}{{5-41=array1.swapAt(i + 5, j - 2)}}
    // expected-note@+1{{conflicting access is here}}
    swap(&array1[i + 5], &array1[j - 2])

    // expected-error@+2{{simultaneous accesses}}{{5-49=self.arrayProp.swapAt(i, j)}}
    // expected-note@+1{{conflicting access is here}}
    swap(&self.arrayProp[i], &self.arrayProp[j])

    var localOfGenericType = param
    // expected-error@+2{{simultaneous accesses}}{{5-75=localOfGenericType.swapAt(paramIndex, paramIndex)}}
    // expected-note@+1{{conflicting access is here}}
    swap(&localOfGenericType[paramIndex], &localOfGenericType[paramIndex])

    // expected-error@+2{{simultaneous accesses}}{{5-39=array1.swapAt(i, j)}}
    // expected-note@+1{{conflicting access is here}}
    Swift.swap(&array1[i], &array1[j]) // no-crash
  }

  mutating
  func shouldHaveNoFixIts(_ i: Int, _ j: Int) {
    var s = StructWithField()
    // expected-error@+2{{simultaneous accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    swap(&s.f, &s.f)

    var array1 = [1, 2, 3]
    var array2 = [1, 2, 3]

    // Swapping between different arrays should cannot have the
    // Fix-It.
    swap(&array1[i], &array2[j]) // no-warning no-fixit
    swap(&array1[i], &self.arrayProp[j]) // no-warning no-fixit

    // Dictionaries aren't MutableCollections so don't support swapAt().
    // expected-error@+2{{simultaneous accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    swap(&dictionaryProp[i], &dictionaryProp[j])

    // We could safely Fix-It this but don't now because the left and
    // right bases are not textually identical.
    // expected-error@+2{{simultaneous accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    swap(&self.arrayProp[i], &arrayProp[j])

    // We could safely Fix-It this but we're not that heroic.
    // We don't suppress when swap() is used as a value
    let mySwap: (inout Int, inout Int) -> () = swap

    // expected-error@+2{{simultaneous accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    mySwap(&array1[i], &array1[j])

    func myOtherSwap<T>(_ a: inout T, _ b: inout T) {
      swap(&a, &b) // no-warning
    }

    // expected-error@+2{{simultaneous accesses}}{{none}}
    // expected-note@+1{{conflicting access is here}}
    mySwap(&array1[i], &array1[j])
  }
}

func inoutSeparateStructStoredProperties() {
  var s = StructWithTwoStoredProp()
  takesTwoInouts(&s.f1, &s.f2) // no-error
}

func inoutSameStoredProperty() {
  var s = StructWithTwoStoredProp()
  takesTwoInouts(&s.f1, &s.f1)
  // expected-error@-1{{simultaneous accesses to var 's.f1', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

func inoutSeparateTupleElements() {
  var t = (1, 4)
  takesTwoInouts(&t.0, &t.1) // no-error
}

func inoutSameTupleElement() {
  var t = (1, 4)
  takesTwoInouts(&t.0, &t.0) // no-error
  // expected-error@-1{{simultaneous accesses to var 't.0', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

func inoutSameTupleNamedElement() {
  var t = (name1: 1, name2: 4)
  takesTwoInouts(&t.name2, &t.name2) // no-error
  // expected-error@-1{{simultaneous accesses to var 't.name2', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}

func inoutSamePropertyInSameTuple() {
  var t = (name1: 1, name2: StructWithTwoStoredProp())
  takesTwoInouts(&t.name2.f1, &t.name2.f1) // no-error
  // expected-error@-1{{simultaneous accesses to var 't.name2.f1', but modification requires exclusive access; consider copying to a local variable}}
  // expected-note@-2{{conflicting access is here}}
}
