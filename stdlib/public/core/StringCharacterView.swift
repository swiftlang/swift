//===--- StringCharacterView.swift - String's Collection of Characters ----===//
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
//  String is-not-a SequenceType or CollectionType, but it exposes a
//  collection of characters.
//
//===----------------------------------------------------------------------===//

extension String {
  public struct CharacterView {
    internal var _core: _StringCore
    
    public init(_ s: String) {
      self._core = s._core
    }
    
    public // @testable
    init(_ _core: _StringCore) {
      self._core = _core
    }
  }

  public var characters: CharacterView {
    return CharacterView(self)
  }

  public mutating func withMutableCharacters<R>(body: (inout CharacterView)->R) -> R {
    var tmp = CharacterView("")
    swap(&_core, &tmp._core)
    let r = body(&tmp)
    swap(&_core, &tmp._core)
    return r
  }
}

