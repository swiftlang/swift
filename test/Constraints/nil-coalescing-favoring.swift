// RUN: %target-swift-frontend -dump-ast %s | %FileCheck %s

struct B {
  static var _none: B { B() }
}

struct A {
  init(_ other: B) {}
  // CHECK: constructor_decl{{.*}}interface_type="(A.Type) -> (B?) -> A"
  init(_ other: B?) {
    // CHECK: dot_syntax_call_expr type="(B) -> A"
    self.init(other ?? ._none)
  }
}

do {
  class Super {}
  class Sub: Super {}

  func flatMap<R>(_: (Int) -> R?) -> R? {}

  func test() {
    let dict: Dictionary<Int, Sub>
    let sup: Super

    // CHECK: declref_expr type="(consuming Super?, @autoclosure () throws -> Super) throws -> Super" {{.*}} decl="Swift.(file).??
    let x = flatMap { dict[$0] } ?? sup // Ok
    let _: Super = x
  }
}

// Reduced from vapor project. Favoring _only_ an overload of `??` and takes `T?` as a second parameter would result in an invalid solution.
extension Array where Element == UInt8 {
  init?(decodingBase32 str: String) {
    guard let decoded = str.utf8.withContiguousStorageIfAvailable({ Array(decodingBase32: $0) }) ?? Array(decodingBase32: Array(str.utf8)) else { // Ok
      return nil
    }
    self = decoded
  }

  init?<C>(decodingBase32 bytes: C) where C: RandomAccessCollection, C.Element == UInt8, C.Index == Int {
    fatalError()
  }
}

func test_no_incorrect_favoring(v: Int?, o: Int) {
  func ternary<T>(_: T, _: T) -> T { fatalError() }

  func nilCoelesing<T>(_: T?, _: T) -> T { fatalError() }
  func nilCoelesing<T>(_: T?, _: T?) -> T? { fatalError() }

  let t1 = v ?? (true ? nil : v)
  let t2 = v ?? ternary(nil, o)

  let s1 = nilCoelesing(v, (true ? nil : v))
  let s2 = nilCoelesing(v, ternary(nil, o))

  func sameType<T>(_: T, as: T.Type) {}

  sameType(t1, as: Int?.self)
  sameType(t2, as: Int?.self)
  sameType(s1, as: Int?.self)
  sameType(s2, as: Int?.self)
}
