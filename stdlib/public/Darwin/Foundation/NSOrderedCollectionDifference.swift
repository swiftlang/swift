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

@_exported import Foundation // Clang module

// CollectionDifference<ChangeElement>.Change is conditionally bridged to NSOrderedCollectionChange
@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
extension CollectionDifference.Change : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSOrderedCollectionChange {
    switch self {
    case .insert(offset: let o, element: let e, associatedWith: let a):
      return NSOrderedCollectionChange(object: e, type: .insert, index: o, associatedIndex: a ?? NSNotFound)
    case .remove(offset: let o, element: let e, associatedWith: let a):
      return NSOrderedCollectionChange(object: e, type: .remove, index: o, associatedIndex: a ?? NSNotFound)
    }
  }
  
  public static func _forceBridgeFromObjectiveC(_ input: NSOrderedCollectionChange, result: inout CollectionDifference.Change?) {
    let _ = input.object as! ChangeElement

    if !_conditionallyBridgeFromObjectiveC(input, result: &result) {
      fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
    }
  }
  
  public static func _conditionallyBridgeFromObjectiveC(
    _ x: NSOrderedCollectionChange, result: inout CollectionDifference.Change?
  ) -> Bool {
    guard let element = x.object as? ChangeElement else { return false }

    let a: Int?
    if x.associatedIndex == NSNotFound {
      a = nil
    } else {
      a = x.associatedIndex
    }

    switch x.changeType {
    case .insert:
      result = .insert(offset: x.index, element: element, associatedWith: a)
    case .remove:
      result = .remove(offset: x.index, element: element, associatedWith: a)
    default:
      return false
    }

    return true
  }
  
  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(_ s: NSOrderedCollectionChange?) -> CollectionDifference.Change {
    var result: CollectionDifference<ChangeElement>.Change? = nil
    CollectionDifference<ChangeElement>.Change._forceBridgeFromObjectiveC(s!, result: &result)
    return result!
  }
}

// CollectionDifference<ChangeElement> is conditionally bridged to NSOrderedCollectionDifference
@available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *)
extension CollectionDifference : _ObjectiveCBridgeable {
  @_semantics("convertToObjectiveC")
  public func _bridgeToObjectiveC() -> NSOrderedCollectionDifference {
    return NSOrderedCollectionDifference(changes: self.map({ $0 as NSOrderedCollectionChange }))
  }
  
  public static func _forceBridgeFromObjectiveC(_ input: NSOrderedCollectionDifference, result: inout CollectionDifference?) {
    if !_conditionallyBridgeFromObjectiveC(input, result: &result) {
      fatalError("Unable to bridge \(_ObjectiveCType.self) to \(self)")
    }
  }

  private static func _formDifference(
    from input: NSOrderedCollectionDifference,
    _ changeConverter: (Any) -> CollectionDifference<ChangeElement>.Change?
  ) -> CollectionDifference<ChangeElement>? {
    var changes = Array<Change>()
    let iteratorSeq = IteratorSequence(NSFastEnumerationIterator(input))
    for objc_change in iteratorSeq {
      guard let swift_change = changeConverter(objc_change) else { return nil }
      changes.append(swift_change)
    }
    return CollectionDifference(changes)
  }

  public static func _conditionallyBridgeFromObjectiveC(
    _ input: NSOrderedCollectionDifference, result: inout CollectionDifference?
  ) -> Bool {
    result = _formDifference(from: input) { $0 as? Change }
    return result != nil
  }
  
  @_effects(readonly)
  public static func _unconditionallyBridgeFromObjectiveC(_ s: NSOrderedCollectionDifference?) -> CollectionDifference {
    return _formDifference(from: s!) { ($0 as! Change) }!
  }
}
