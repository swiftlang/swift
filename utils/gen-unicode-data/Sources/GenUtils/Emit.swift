//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

func _eytzingerize<C: Collection>(_ collection: C, result: inout [C.Element], sourceIndex: Int, resultIndex: Int) -> Int where C.Element: Comparable, C.Index == Int {
  var sourceIndex = sourceIndex
  if resultIndex < result.count {
    sourceIndex = _eytzingerize(collection, result: &result, sourceIndex: sourceIndex, resultIndex: 2 * resultIndex)
    result[resultIndex] = collection[sourceIndex]
    sourceIndex = _eytzingerize(collection, result: &result, sourceIndex: sourceIndex + 1, resultIndex: 2 * resultIndex + 1)
  }
  return sourceIndex
}

/*
 Takes a sorted collection and reorders it to an array-encoded binary search tree, as originally developed by Michaël Eytzinger in the 16th century.
 This allows binary searching the array later to touch roughly 4x fewer cachelines, significantly speeding it up.
 */
public func eytzingerize<C: Collection>(_ collection: C, dummy: C.Element) -> [C.Element] where C.Element: Comparable, C.Index == Int {
  var result = Array(repeating: dummy, count: collection.count + 1)
  _ = _eytzingerize(collection, result: &result, sourceIndex: 0, resultIndex: 1)
  return result
}

public func emitCollection<C: Collection>(
  _ collection: C,
  name: String,
  type: String,
  into result: inout String,
  formatter: (C.Element) -> String
) {
  result += """
  static const \(type) \(name)[\(collection.count)] = {
  
  """
  
  formatCollection(collection, into: &result, using: formatter)
  
  result += "\n};\n\n"
}

public func emitCollection<C: Collection>(
  _ collection: C,
  name: String,
  into result: inout String
) where C.Element: FixedWidthInteger {
  emitCollection(
    collection,
    name: name,
    type: "__swift_\(C.Element.isSigned ? "" : "u")int\(C.Element.bitWidth)_t",
    into: &result
  ) {
    "0x\(String($0, radix: 16, uppercase: true))"
  }
}

// Emits an abstract minimal perfect hash function into C arrays.
public func emitMph(
  _ mph: Mph,
  name: String,
  defineLabel: String,
  into result: inout String
) {
  result += """
  #define \(defineLabel)_LEVEL_COUNT \(mph.bitArrays.count)
  
  
  """
  
  emitMphSizes(mph, name, into: &result)
  emitMphBitarrays(mph, name, into: &result)
  emitMphRanks(mph, name, into: &result)
}

// BitArray sizes
func emitMphSizes(_ mph: Mph, _ name: String, into result: inout String) {
  emitCollection(
    mph.bitArrays,
    name: "\(name)_sizes",
    type: "__swift_uint16_t",
    into: &result
  ) {
    "0x\(String($0.size, radix: 16, uppercase: true))"
  }
}

func emitMphBitarrays(_ mph: Mph, _ name: String, into result: inout String) {
  // Individual bitarrays
  
  for (i, ba) in mph.bitArrays.enumerated() {
    emitCollection(ba.words, name: "\(name)_keys\(i)", into: &result)
  }
  
  // Overall bitarrays
  
  emitCollection(
    mph.bitArrays.indices,
    name: "\(name)_keys",
    type: "__swift_uint64_t * const",
    into: &result
  ) {
    "\(name)_keys\($0)"
  }
}

func emitMphRanks(_ mph: Mph, _ name: String, into result: inout String) {
  // Individual ranks
  
  for (i, rank) in mph.ranks.enumerated() {
    emitCollection(rank, name: "\(name)_ranks\(i)", into: &result)
  }
  
  // Overall ranks
  
  emitCollection(
    mph.ranks.indices,
    name: "\(name)_ranks",
    type: "__swift_uint16_t * const",
    into: &result
  ) {
    "\(name)_ranks\($0)"
  }
}
