//===--- StringUTF16.swift ------------------------------------------------===//
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
extension String {
  @public struct UTF16View : Sliceable {
    @public var startIndex: Int {
      return _core.startIndex
    }
    @public var endIndex: Int {
      return _core.endIndex
    }

    // This is to avoid printing "func generate() -> IndexingGenerator<_StringCore>"
    @public typealias _GeneratorType = _StringCore.GeneratorType
    @public typealias GeneratorType = _GeneratorType

    @public func generate() -> GeneratorType {
      return _core.generate()
    }
    @public subscript(i: Int) -> GeneratorType.Element {
      return _core[i]
    }
    @public subscript(subRange: Range<Int>) -> UTF16View {
      return UTF16View(_core[subRange])
    }
    
    init(_ _core: _StringCore) {
      self._core = _core
    }
    
    let _core: _StringCore
  }

  @public var utf16: UTF16View {
    return UTF16View(core)
  }
}
