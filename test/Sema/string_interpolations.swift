// RUN: %target-typecheck-verify-swift

do {
  enum E: String, CaseIterable {
    static var allCases: [E] {
      [.one, .two]
    }

    case one
    case two

    func test(id: String) {
      for c in Self.allCases where id == "\(c)" { // Ok
      }
    }
  }
}

do {
  struct Data {
    var text: String

    static func fn(_: (inout Data) -> Void) {}
    static func fn(_: (inout Int) -> Void) {}
  }

  func test<T>(_: @autoclosure () -> T) {
  }

  test((1...3).map { number in Data.fn { $0.text = "\(number)" } })

  func test_multi<T>(_: () -> T) {
  }

  test_multi {
    let x = 1...3
    _ = x.map { number in
      Data.fn {
        if $0.text == "\(number)" {
          $0.text = "\(x)"
        }
      }
    }
  }
}
