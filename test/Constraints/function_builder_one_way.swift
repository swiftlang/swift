// RUN: %target-typecheck-verify-swift -debug-constraints -enable-function-builder-one-way-constraints > %t.log 2>&1
// RUN: %FileCheck %s < %t.log

enum Either<T,U> {
  case first(T)
  case second(U)
}

@_functionBuilder
struct TupleBuilder {
  static func buildBlock<T1>(_ t1: T1) -> T1 {
    return t1
  }

  static func buildBlock<T1, T2>(_ t1: T1, _ t2: T2) -> (T1, T2) {
    return (t1, t2)
  }
  
  static func buildBlock<T1, T2, T3>(_ t1: T1, _ t2: T2, _ t3: T3)
      -> (T1, T2, T3) {
    return (t1, t2, t3)
  }

  static func buildBlock<T1, T2, T3, T4>(_ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4)
      -> (T1, T2, T3, T4) {
    return (t1, t2, t3, t4)
  }

  static func buildBlock<T1, T2, T3, T4, T5>(
    _ t1: T1, _ t2: T2, _ t3: T3, _ t4: T4, _ t5: T5
  ) -> (T1, T2, T3, T4, T5) {
    return (t1, t2, t3, t4, t5)
  }

  static func buildDo<T>(_ value: T) -> T { return value }
  static func buildIf<T>(_ value: T?) -> T? { return value }

  static func buildEither<T,U>(first value: T) -> Either<T,U> {
    return .first(value)
  }
  static func buildEither<T,U>(second value: U) -> Either<T,U> {
    return .second(value)
  }
}

func tuplify<C: Collection, T>(_ collection: C, @TupleBuilder body: (C.Element) -> T) -> T {
  return body(collection.first!)
}

// CHECK: ---Connected components---
// CHECK-NEXT: 0: $T1 $T2 $T3 $T5 $T6 $T7 $T8 $T69 depends on 1
// CHECK-NEXT: 1: $T9 $T10 $T11 $T12 $T13 $T14 $T16 $T18 $T21 $T35 $T67 $T68 depends on 2, 3, 4, 5, 6
// CHECK-NEXT: 6: $T37 $T38 $T39 $T50 $T51 $T52 $T53 $T54 $T64 $T65 $T66 depends on 7, 10
// CHECK-NEXT: 10: $T55 $T56 $T62 $T63 depends on 11
// CHECK-NEXT: 11: $T57 $T58 $T59 $T60 $T61
// CHECK-NEXT: 7: $T40 $T41 $T42 $T44 $T48 $T49 depends on 8, 9
// CHECK-NEXT: 9: $T45 $T46 $T47
// CHECK-NEXT: 8: $T43
// CHECK-NEXT: 5: $T22 $T23 $T24 $T25 $T26 $T27 $T28 $T29 $T30 $T31 $T32 $T33 $T34
// CHECK-NEXT: 4: $T19 $T20
// CHECK-NEXT: 3: $T17
// CHECK-NEXT: 2: $T15
let names = ["Alice", "Bob", "Charlie"]
let b = true
print(
  tuplify(names) { name in
    17
    3.14159
    "Hello, \(name)"
    tuplify(["a", "b"]) { value in
      value.first!
    }
    if b {
      2.71828
      ["if", "stmt"]
    } else {
      [1, 2, 3, 4]
    }
  })
