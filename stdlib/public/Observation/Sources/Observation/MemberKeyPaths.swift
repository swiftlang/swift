//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.9, *)
@frozen
public struct MemberKeyPaths<Root>: SetAlgebra, Hashable {
  public typealias Element = PartialKeyPath<Root>
  public typealias ArrayLiteralElement = PartialKeyPath<Root>
  
  @usableFromInline
  internal var raw: Set<UnsafeRawPointer>
  
  @inlinable
  internal init(raw keyPaths: Set<UnsafeRawPointer>) {
    self.raw = keyPaths
  }
  
  @inlinable
  public init(_ sequence: __owned some Sequence<PartialKeyPath<Root>>) {
    self.raw = Set(sequence.map { 
      UnsafeRawPointer(Unmanaged.passUnretained($0).toOpaque()) 
    })
  }
  
  @inlinable
  public init() {
    self.raw = Set()
  }
  
  @inlinable
  public init(arrayLiteral elements: PartialKeyPath<Root>...) {
    self.init(elements)
  }
  
  @inlinable
  public func union(
    _ other: __owned MemberKeyPaths<Root>
  ) -> MemberKeyPaths<Root> {
    MemberKeyPaths(raw: raw.union(other.raw))
  }
  
  @inlinable
  public func intersection(
    _ other: MemberKeyPaths<Root>
  ) -> MemberKeyPaths<Root> {
    MemberKeyPaths(raw: raw.intersection(other.raw))
  }
  
  @inlinable
  public func symmetricDifference(
    _ other: __owned MemberKeyPaths<Root>
  ) -> MemberKeyPaths<Root> {
    MemberKeyPaths(raw: raw.symmetricDifference(other.raw))
  }
  
  @inlinable
  public mutating func formUnion(_ other: __owned MemberKeyPaths<Root>) {
    raw.formUnion(other.raw)
  }
  
  @inlinable
  public mutating func formIntersection(_ other: MemberKeyPaths<Root>) {
    raw.formIntersection(other.raw)
  }
  
  @inlinable
  public mutating func formSymmetricDifference(
    _ other: __owned MemberKeyPaths<Root>
  ) {
    raw.formSymmetricDifference(other.raw)
  }
  
  @inlinable
  public func contains(_ member: PartialKeyPath<Root>) -> Bool {
    raw.contains(UnsafeRawPointer(Unmanaged.passUnretained(member).toOpaque()))
  }
  
  @inlinable
  public var isEmpty: Bool {
    raw.isEmpty
  }
  
  @inlinable
  @discardableResult
  public mutating func insert(
    _ newMember: __owned PartialKeyPath<Root>
  ) -> (inserted: Bool, memberAfterInsert: PartialKeyPath<Root>) {
    let (inserted, memberAfterInsert) = raw.insert(
      UnsafeRawPointer(Unmanaged.passUnretained(newMember).toOpaque()))
    return (
      inserted, 
      Unmanaged<PartialKeyPath<Root>>.fromOpaque(memberAfterInsert)
        .takeUnretainedValue()
    )
  }
  
  @inlinable
  internal mutating func _insert(_ raw: UnsafeRawPointer) {
    self.raw.insert(raw)
  }
  
  @inlinable
  @discardableResult
  public mutating func remove(
    _ member: PartialKeyPath<Root>
  ) -> PartialKeyPath<Root>? {
    raw.remove(
      UnsafeRawPointer(Unmanaged.passUnretained(member).toOpaque()))
      .map { Unmanaged.fromOpaque($0).takeUnretainedValue() }
  }
  
  @inlinable
  @discardableResult
  public mutating func update(
    with newMember: __owned PartialKeyPath<Root>
  ) -> PartialKeyPath<Root>? {
    raw.update(with: 
      UnsafeRawPointer(Unmanaged.passUnretained(newMember).toOpaque()))
      .map { Unmanaged.fromOpaque($0).takeUnretainedValue() }
  }
  
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(raw)
  }
  
  @inlinable
  public static func == (
    _ lhs: MemberKeyPaths<Root>, 
    _ rhs: MemberKeyPaths<Root>
  ) -> Bool {
    return lhs.raw == rhs.raw
  }
}

@available(SwiftStdlib 5.9, *)
extension MemberKeyPaths where Root: Observable {
  @inlinable
  public init(members: MemberKeyPaths<Root>, root: Root) {
    self.init()
    for raw in members.raw {
      self.formUnion(
        root.memberKeyPaths(for: 
          Unmanaged<PartialKeyPath<Root>>.fromOpaque(raw)
            .takeUnretainedValue()))
    }
  }
}
