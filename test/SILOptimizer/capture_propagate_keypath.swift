// RUN: %target-swift-frontend  -primary-file %s -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s
// RUN: %target-swift-frontend  -primary-file %s -swift-version 6 -O -sil-verify-all -module-name=test -emit-sil | %FileCheck %s

// Also do an end-to-end test to check if the generated code is correct.
// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT
// REQUIRES: executable_test

struct Str {
  var i: Int = 27
  var j: Int = 28
  @inline(never)
  var c: Int { i + 100 }
}

struct GenStr<T> {
  var i: T
  var j: T
  var x: T
  @inline(never)
  var c: T { return x }

  init(_ t1: T, _ t2: T, _ t3: T) {
    self.i = t1
    self.j = t2
    self.x = t3
  }
}

struct GetID<C: RandomAccessCollection, ID> {
  var getID: (C.Element) -> ID

  @inline(__always)
  init(id: KeyPath<C.Element, ID>) {
    getID = { $0[keyPath: id] }
  }
}

// CHECK-LABEL: sil {{.*}} @$s4test0A6SimpleyyySiAA3StrVXEXEF :
// CHECK-NOT:     keypath
// CHECK-LABEL:  } // end sil function '$s4test0A6SimpleyyySiAA3StrVXEXEF'
@inline(never)
func testSimple(_ mymap: ((Str)->Int) -> ()) {
  mymap(\.i)
  mymap(\.j)
  mymap(\.c)
}

// CHECK-LABEL: sil {{.*}} @$s4test0A6GenStryyySiAA0bC0VySiGXEXEF :
// CHECK-NOT:     keypath
// CHECK-LABEL:  } // end sil function '$s4test0A6GenStryyySiAA0bC0VySiGXEXEF'
@inline(never)
func testGenStr(_ mymap: ((GenStr<Int>)->Int) -> ()) {
  mymap(\.i)
  mymap(\.j)
  mymap(\.c)
}

// CHECK-LABEL: sil {{.*}} @$s4test0A22GenericEscapingClosureAA5GetIDVySayAA3StrVGSiGyF :
// CHECK-NOT:     keypath
// CHECK:         = function_ref @[[SPECIALIZED_CLOSURE:.*]] : $@convention(thin) (@in_guaranteed Str) -> @out Int
// CHECK-NOT:     keypath
// CHECK-LABEL:  } // end sil function '$s4test0A22GenericEscapingClosureAA5GetIDVySayAA3StrVGSiGyF'
@inline(never)
func testGenericEscapingClosure() -> GetID<[Str], Int> {
    GetID(id: \.i)
} 

// CHECK-LABEL: sil {{.*}} @$s4test0A7GenericyyyxAA6GenStrVyxGXEXElF :
// CHECK:         keypath
// CHECK:         keypath
// CHECK:         keypath
// CHECK:       } // end sil function '$s4test0A7GenericyyyxAA6GenStrVyxGXEXElF'
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func testGeneric<T>(_ mymap: ((GenStr<T>)->T) -> ()) {
  mymap(\.i)
  mymap(\.j)
  mymap(\.c)
}

@inline(never)
@_optimize(none)
public func _opaqueIdentity<T>(_ x: T) -> T {
  return x
}

// CHECK:       sil shared {{.*}}@[[SPECIALIZED_CLOSURE]] :
// CHECK-NOT:     keypath
// CHECK:       } // end sil function '[[SPECIALIZED_CLOSURE]]'

func calltests() {
  // CHECK-OUTPUT-LABEL: testSimple:
  print("testSimple:")

  // CHECK-OUTPUT-NEXT:  27
  // CHECK-OUTPUT-NEXT:  28
  // CHECK-OUTPUT-NEXT:  127
  testSimple { (c: (Str)->Int) in
    let s = Str()
    print(c(s))
  }

  // CHECK-OUTPUT-LABEL: testGenStr:
  print("testGenStr:")

  // CHECK-OUTPUT-NEXT:  3
  // CHECK-OUTPUT-NEXT:  4
  // CHECK-OUTPUT-NEXT:  5
  testGenStr { (c: (GenStr)->Int) in
    let s = GenStr(3, 4, 5)
    print(c(s))
  }

  // CHECK-OUTPUT-LABEL: testGeneric:
  print("testGeneric:")

  // CHECK-OUTPUT-NEXT:  3
  // CHECK-OUTPUT-NEXT:  4
  // CHECK-OUTPUT-NEXT:  5
  testGeneric { (c: (GenStr)->Int) in
    let s = GenStr(3, 4, 5)
    print(c(s))
  }

  // CHECK-OUTPUT-LABEL: testGenericEscapingClosure:
  print("testGenericEscapingClosure:")

  // CHECK-OUTPUT-NEXT:  27
  print(testGenericEscapingClosure().getID(Str()))
}

calltests()

