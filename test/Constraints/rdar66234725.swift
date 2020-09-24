// RUN: %target-typecheck-verify-swift

protocol P {}

protocol Func {
  associatedtype Result

  init()

  mutating func update<D: P>(data: D)

  func finalize() -> Result
}

struct S<T: P, F: Func> {
  var arr: [T]

  func test() -> [F.Result] {
    return stride(from: 0, to: arr.endIndex, by: 2).map {
      (arr[$0], $0 < arr.index(before: arr.endIndex) ? arr[$0.advanced(by: 1)] : nil) // Ok
    }.map { (lhs, rhs) -> F.Result in
      var fun = F()
      fun.update(data: lhs)
      fun.update(data: rhs!)
      return fun.finalize()
    }
  }
}
