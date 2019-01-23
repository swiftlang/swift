// RUN: %target-swift-frontend -emit-ir -g -o - %s 

// REQUIRES: objc_interop

import Foundation
public final class Foo: NSObject, Collection {
  public var storage = [String: Any]()
  
  public typealias DictionaryType = [String: Any]
  public typealias IndexDistance = Int
  public typealias Indices = DictionaryType.Indices
  public typealias Iterator = DictionaryType.Iterator
  public typealias SubSequence = DictionaryType.SubSequence
  public typealias Index = DictionaryType.Index
  
  public var startIndex: Index { return storage.startIndex }
  public var endIndex: DictionaryType.Index { return storage.endIndex }
  public subscript(position: Index) -> Iterator.Element {
    return storage[position]
  }
  
  public subscript(bounds: Range<Index>) -> SubSequence {
    return storage[bounds]
  }
  
  public var indices: Indices { return storage.indices }
  
  public subscript(key: String) -> Any? {
    get { return storage[key] }
    set { storage[key] = newValue }
  }
  
  public func index(after i: Index) -> Index { return storage.index(after: i) }
  
  public func makeIterator() -> DictionaryIterator<String, Any> {
    return storage.makeIterator()
  }
  
  public override func value(forKey key: String) -> Any? {
    return storage[key]
  }

  public override func value(forUndefinedKey key: String) -> Any? {
    return storage[key]
  }
}
