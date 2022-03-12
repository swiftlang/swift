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
