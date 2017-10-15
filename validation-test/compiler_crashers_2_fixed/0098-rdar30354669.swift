// RUN: %target-swift-frontend -emit-ir -primary-file %s

public func ==<T: EquatableMetaType>(lhs: T, rhs: T) -> Bool {
  return type(of: lhs) == type(of: lhs)
}


public protocol EquatableMetaType {
}


class Block : Equatable, EquatableMetaType {
}

extension Array where Element : Block {
  func indexByType(of: Element) -> Array.Index? {
    let count = self.count
    var result: Array.Index = 0

    for i in self {
      if i == of {
        return result
      }
      if result < count {
        result += 1
      }
    }
    return nil
  }
}
