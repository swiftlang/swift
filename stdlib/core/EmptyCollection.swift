//===--- EmptyCollection.swift - A collection with no elements ------------===//
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
//
//  Sometimes an operation is best expressed in terms of some other,
//  larger operation where one of the parameters is an empty
//  collection.  For example, we can erase elements from an Array by
//  replacing a subrange with the empty collection.
//
//===----------------------------------------------------------------------===//

@public struct EmptyGenerator<T> : Generator, Sequence, Reflectable {
  @public func generate() -> EmptyGenerator {
    return self
  }
  
  @public mutating func next() -> T? {
    return nil
  }
  
  @public func getMirror() -> Mirror {
    return _EmptyGeneratorMirror(self)
  }
}

struct _EmptyGeneratorMirror<T>: Mirror {
  let _value: EmptyGenerator<T>
  
  init(_ val: EmptyGenerator<T>) {
    self._value = val
  }
  
  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 0 }

  subscript(i: Int) -> (String, Mirror) {
    _preconditionFailure("cannot extract this child index")
  }

  var summary: String { 
    return "EmptyGenerator"
  }

  var quickLookObject: QuickLookObject? { return .None }

  var disposition: MirrorDisposition { return .Struct }
}

@public struct EmptyCollection<T> : Collection, Reflectable {
  @public typealias IndexType = Int
  
  @public var startIndex: IndexType {
    return 0
  }
  
  @public var endIndex: IndexType {
    return 0
  }

  @public func generate() -> EmptyGenerator<T> {
    return EmptyGenerator()
  }

  @public subscript(i: IndexType) -> T {
    _preconditionFailure("Index out of range")
  }
  
  @public func getMirror() -> Mirror {
    return _EmptyCollectionMirror(self)
  }
}

struct _EmptyCollectionMirror<T>: Mirror {
  let _value: EmptyCollection<T>
  
  init(_ val: EmptyCollection<T>) {
    self._value = val
  }
  
  var value: Any { return _value }

  var valueType: Any.Type { return (_value as Any).dynamicType }

  var objectIdentifier: ObjectIdentifier? { return .None }

  var count: Int { return 0 }

  subscript(i: Int) -> (String, Mirror) {
    _preconditionFailure("cannot extract this child index")
  }

  var summary: String { 
    return "EmptyCollection"
  }

  var quickLookObject: QuickLookObject? { return .None }

  var disposition: MirrorDisposition { return .Struct }
}

// Specialization of countElements for EmptyCollection<T>
@public func ~> <T>(x:EmptyCollection<T>, _:(_CountElements, ())) -> Int {
  return 0
}
