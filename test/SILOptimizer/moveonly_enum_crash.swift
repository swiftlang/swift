// RUN: %target-swift-frontend -emit-sil -verify %s
// https://github.com/swiftlang/swift/issues/87406

public enum List<Element> {
      case empty
      indirect case list(Element, List)
}

func cons(_ value: Int, _ tail: consuming List<Int>) -> List<Int> {
      return .list(value, tail)
}
