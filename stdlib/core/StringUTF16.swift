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
  struct UTF16View : Sliceable {
    var startIndex: Int {
      return _core.startIndex
    }
    var endIndex: Int {
      return _core.endIndex
    }

    // This is to avoid printing "func generate() -> IndexingGenerator<_StringCore>"
    typealias _GeneratorType = _StringCore.GeneratorType
    typealias GeneratorType = _GeneratorType

    func generate() -> GeneratorType {
      return _core.generate()
    }
    subscript(i: Int) -> GeneratorType.Element {
      return _core[i]
    }
    subscript(subRange: Range<Int>) -> UTF16View {
      return UTF16View(_core[subRange])
    }
    
    init(_ _core: _StringCore) {
      self._core = _core
    }
    
    let _core: _StringCore
  }

  var utf16: UTF16View {
    return UTF16View(core)
  }
}
