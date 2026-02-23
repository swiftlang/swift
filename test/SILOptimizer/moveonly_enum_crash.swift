// RUN: %target-swift-frontend -emit-sil -verify %s

public enum List<Element> {
      case empty
      indirect case list(Element, List)
}

func cons(_ value: Int, _ tail: consuming List<Int>) -> List<Int> {
      return .list(value, tail)
}
