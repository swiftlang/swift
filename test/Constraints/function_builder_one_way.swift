// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -debug-constraints > %t.log 2>&1
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
// CHECK-NEXT:   1: $T10 depends on 0
// CHECK-NEXT:   0: $T1 $T2 $T3 $T5 $T6 $T7 $T8 $T77 $T78 depends on 3
// CHECK-NEXT:   3: $T12 $T17 $T28 $T42 $T53 $T54 $T55 $T56 $T57 $T58 $T59 $T60 $T61 $T62 $T63 $T64 $T65 $T66 $T68 $T69 $T70 $T71 $T72 $T73 $T74 $T75 $T76 depends on 2, 4, 5, 7, 10
// CHECK-NEXT:   10: $T48 $T49 $T50 $T51 $T52 depends on 9
// CHECK-NEXT:   9: $T43 $T44 $T45 $T46 $T47
// CHECK-NEXT:   7: $T31 $T35 $T36 $T37 $T38 $T39 $T40 $T41 depends on 6, 8
// CHECK-NEXT:   8: $T32 $T33 $T34
// CHECK-NEXT:   6: $T30
// CHECK-NEXT:   5: $T18 $T19 $T20 $T21 $T22 $T23 $T24 $T25 $T26 $T27
// CHECK-NEXT:   4: $T15 $T16
// CHECK-NEXT:   2: $T11
let names = ["Alice", "Bob", "Charlie"]
let b = true
var number = 17
print(
  tuplify(names) { name in
    17
    number
    "Hello, \(name)"
    tuplify(["a", "b"]) { value in
      value.first!
    }
    if b {
      2.71828
      ["if", "stmt"]
    } else {
      [1, 2, 3, 17]
    }
  })
