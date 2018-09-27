//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
// Convenience APIs for Set<AnyHashable>
//===----------------------------------------------------------------------===//

extension Set where Element == AnyHashable {
  @inlinable
  public mutating func insert<ConcreteElement : Hashable>(
    _ newMember: __owned ConcreteElement
  ) -> (inserted: Bool, memberAfterInsert: ConcreteElement) {
    let (inserted, memberAfterInsert) =
      insert(AnyHashable(newMember))
    return (
      inserted: inserted,
      memberAfterInsert: memberAfterInsert.base as! ConcreteElement)
  }

  @inlinable
  @discardableResult
  public mutating func update<ConcreteElement : Hashable>(
    with newMember: __owned ConcreteElement
  ) -> ConcreteElement? {
    return update(with: AnyHashable(newMember))
      .map { $0.base as! ConcreteElement }
  }

  @inlinable
  @discardableResult
  public mutating func remove<ConcreteElement : Hashable>(
    _ member: ConcreteElement
  ) -> ConcreteElement? {
    return remove(AnyHashable(member))
      .map { $0.base as! ConcreteElement }
  }
}
