// RUN: %target-typecheck-verify-swift

class Data {
}

class Test {
  var lastIndex: Int = 0

  func test(_ arr: [[Data]]) {
    typealias I = (index: Int, data: [Int])

    struct Pair: Hashable {
      var lhs: Int
      var rhs: Int
    }

    let data = [Pair: I]()
    for index in 0..<arr.count {
      for (idx, _) in arr[index].enumerated() {
        _ = data[Pair(lhs: index, rhs: idx)] ?? I(index: lastIndex, data: [Int]()) // Ok
      }
    }
  }
}
