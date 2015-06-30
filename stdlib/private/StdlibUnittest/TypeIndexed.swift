//===--- TypeIndexed.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

protocol Resettable : AnyObject {
  func reset()
}

internal var _allResettables: [Resettable] = []

public class TypeIndexed<Value> : Resettable {
  public init(_ value: Value) {
    self.defaultValue = value
    _allResettables.append(self)
  }
  
  public subscript(t: Any.Type) -> Value {
    get {
      return byType[TypeIdentifier(t)] ?? defaultValue
    }
    set {
      byType[TypeIdentifier(t)] = newValue
    }
  }

  public func reset() { byType = [:] }

  internal var byType: [TypeIdentifier:Value] = [:]
  internal var defaultValue: Value
}

public func <=> <T: Comparable>(
  lhs: (TypeIdentifier, T),
  rhs: (TypeIdentifier, T)
) -> ExpectedComparisonResult {
  let a = lhs.0 <=> rhs.0
  if !a.isEQ() { return a }
  return lhs.1 <=> rhs.1
}

public func expectEqual<V: Comparable>(
  expected: DictionaryLiteral<Any.Type, V>, _ actual: TypeIndexed<V>,
  stackTrace: SourceLocStack? = nil,
  file: String = __FILE__, line: UWord = __LINE__,
  collectMoreInfo: (()->String)? = nil
) {
  expectEqualsUnordered(
    expected.map { (TypeIdentifier($0.0), $0.1) },
    actual.byType,
    { $0 <=> $1 },
    stackTrace: stackTrace, file: file,
    line: line, collectMoreInfo: collectMoreInfo
  )
}

