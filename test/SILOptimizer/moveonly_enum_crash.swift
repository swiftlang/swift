// RUN: %target-swift-frontend -emit-sil -verify %s
// https://github.com/swiftlang/swift/issues/87406

public enum List<Element> {
      case empty
      indirect case list(Element, List)
      
      consuming func performAction() {}
      
      var prop: Int {
          consuming get { return 0 }
      }
}

func cons(_ value: Int, _ tail: consuming List<Int>) -> List<Int> {
      return .list(value, tail)
}

func testListMethodAndProperty(_ x: consuming List<Int>) {
      x.performAction()
      _ = x.prop
}

public enum MoveOnlyList<Element>: ~Copyable {
      case empty
      indirect case list(Element, MoveOnlyList)

      consuming func performAction() {}
      
      var prop: Int {
          consuming get { return 0 }
      }
}

func testMoveOnlyListMethod(_ tail: consuming MoveOnlyList<Int>) {
      tail.performAction() // expected-note {{consumed here}}
      tail.performAction() // expected-error {{'tail' used after consume}}
}

func testMoveOnlyListProperty(_ tail: consuming MoveOnlyList<Int>) {
      let _ = tail.prop // expected-note {{consumed here}}
      let _ = tail.prop // expected-error {{'tail' used after consume}}
}

func testMoveOnlyListMultipleConsume(_ value: Int, _ tail: consuming MoveOnlyList<Int>) -> MoveOnlyList<Int> {
      let _ = tail.prop // expected-note {{consumed here}}
      return .list(value, tail) // expected-error {{'tail' used after consume}}
}
