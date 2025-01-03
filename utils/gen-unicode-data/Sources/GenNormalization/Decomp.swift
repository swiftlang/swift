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

import GenUtils

enum DecompositionKind {
  case canonical
  case compatibility
}

typealias ParsedDecompositions = [(scalar: UInt32, decomposition: [UInt32])]

/// Parses a list of scalars and their decompositions from a UnicodeData.txt
/// file.
///
/// The format of UnicodeData.txt is described by [UAX#44](UAX44).
///
/// [UAX44]: https://www.unicode.org/reports/tr44/#UnicodeData.txt
///
func parseDecompositionMappings(
  kind: DecompositionKind,
  from data: String
) -> ParsedDecompositions {

  var mappings: ParsedDecompositions = []

  for line in data.split(separator: "\n") {

    // Each line in the data file contains a series of components
    // separated by a ';'. For example:
    //
    //     1B06;BALINESE LETTER AKARA TEDUNG;Lo;0;L;1B05 1B35;;;;N;;;;;
    //
    // Index 0 is the scalar value ("1B06" above).
    // Index 5 is the list of scalars it decomposes to ("1B05 1B35" above).

    let components = line.split(
      separator: ";",
      omittingEmptySubsequences: false
    )

    var decompositionStr = components[5]
    guard !decompositionStr.isEmpty else {
      continue // No decomposition.
    }

    // Compatibility decompositions are prefixed with a formatting tag,
    // such as <circle>, <wide>, or <fraction>. For example:
    //
    //     2460;CIRCLED DIGIT ONE;No;0;ON;<circle> 0031;;1;1;N;;;;;
    //                                    ^^^^^^^^
    //
    // If there is no tag, the decomposition is canonical.

    switch kind {
    case .canonical:
      guard decompositionStr.first != "<" else {
        continue
      }

    case .compatibility:
      guard decompositionStr.first == "<" else {
        continue
      }
      decompositionStr = decompositionStr.drop(while: { $0 != ">" })
      decompositionStr = decompositionStr.dropFirst()
      precondition(
        !decompositionStr.isEmpty,
        "Invalid decomposition mapping in line: \(line)"
      )
    }

    let decomposition = decompositionStr.split(separator: " ").map {
      UInt32($0, radix: 16)!
    }
    
    let scalarStr = components[0]
    let scalar = UInt32(scalarStr, radix: 16)!
    
    mappings.append((scalar, decomposition))
  }

  return mappings
}

/// Returns the recursive expansion of the given decomposition mapping table,
/// as required for full decomposition.
///
/// If an additional table is provided, they must not overlap
/// (no scalar may have a decomposition in both tables).
/// The result is the recursive expansion of the union of both tables.
///
func recursivelyExpand(
  _ rawDecompositions: ParsedDecompositions,
  including additionalDecompositions: ParsedDecompositions? = .none
) -> ParsedDecompositions {

  func decompose(_ scalar: UInt32, into result: inout [UInt32]) {

    // Data in the ParsedDecompositions table is sorted by scalar,
    // so cut off the search if we exceed the value we're looking for.

    if let rawEntry = rawDecompositions.first(where: { $0.scalar >= scalar }),
       rawEntry.scalar == scalar {
      for scalar in rawEntry.decomposition {
        decompose(scalar, into: &result)
      }
      return
    }

    if let rawEntry = additionalDecompositions?.first(where: { $0.scalar >= scalar }),
       rawEntry.scalar == scalar {
      for scalar in rawEntry.decomposition {
        decompose(scalar, into: &result)
      }
      return
    }

    result.append(scalar)
  }

  // Recursively expand 'rawDecompositions',
  // including decompositions from 'additionalDecompositions' (if given).
  //
  // This keeps the same set of scalar keys, and just expands the values.

  var expandedDecompositions: ParsedDecompositions = rawDecompositions.map {
    (scalar, rawDecomposition) in
    var expandedDecomposition: [UInt32] = []
    for rawDecompositionScalar in rawDecomposition {
      decompose(rawDecompositionScalar, into: &expandedDecomposition)
    }
    return (scalar, expandedDecomposition)
  }

  guard let additionalDecompositions else {
    return expandedDecompositions
  }

  // If we have two decomposition tables, they must not overlap -
  // Every scalar must have a single decomposition in ONE table only.

  var seenScalars = Set<UInt32>(
    minimumCapacity: rawDecompositions.count + additionalDecompositions.count
  )
  for (scalar, _) in rawDecompositions {
    guard seenScalars.insert(scalar).inserted else {
      fatalError("Duplicate entries for scalar: \(scalar)")
    }
  }
  for (scalar, _) in additionalDecompositions {
    guard seenScalars.insert(scalar).inserted else {
      fatalError("Duplicate entries for scalar: \(scalar)")
    }
  }

  // Recursively expand 'additionalDecompositions',
  // including decompositions from both tables.
  //
  // Merge the result in to 'expandedDecompositions'.

  expandedDecompositions += additionalDecompositions.map {
    (scalar, rawDecomposition) in
    var expandedDecomposition: [UInt32] = []
    for rawDecompositionScalar in rawDecomposition {
      decompose(rawDecompositionScalar, into: &expandedDecomposition)
    }
    return (scalar, expandedDecomposition)
  }
  expandedDecompositions.sort(by: { $0.scalar < $1.scalar })

  return expandedDecompositions
}

/// Emits the given decomposition mapping table
/// as a Minimal Perfect Hash table (MPH), contained in static C arrays.
///
func emitDecompositionMappingTable(
  _ data: ParsedDecompositions,
  kind: DecompositionKind,
  into result: inout String
) {

  let mph = mph(for: data.map { UInt64($0.scalar) })
  emitMph(
    mph,
    name: kind.mappingTableBaseName,
    defineLabel: kind.defineLabel,
    into: &result
  )

  let sortedData: [(scalar: UInt32, mphIndex: UInt16)] = data.map { 
    (scalar, _) in
    return (scalar, UInt16(mph.index(for: UInt64(scalar))))
  }.sorted { $0.mphIndex < $1.mphIndex }

  let indices = emitDecompDecomp(data, sortedData, kind: kind, into: &result)
  emitDecompIndices(indices, kind: kind, into: &result)
}

extension DecompositionKind {

  fileprivate var mappingTableBaseName: String {
    switch self {
    case .canonical:
      return "_swift_stdlib_nfd_decomp"
    case .compatibility:
      return "_swift_stdlib_nfkd_decomp"
    }
  }

  fileprivate var defineLabel: String {
    switch self {
    case .canonical:
      return "NFD_DECOMP"
    case .compatibility:
      return "NFKD_DECOMP"
    }
  }
}

private func emitDecompDecomp(
  _ data: ParsedDecompositions,
  _ sortedData: [(scalar: UInt32, mphIndex: UInt16)],
  kind: DecompositionKind,
  into result: inout String
) -> [(UInt32, UInt16)] {

  var indices: [(UInt32, UInt16)] = []
  var decompResult: [UInt8] = []
  
  // Keep a record of decompositions because some scalars share the same
  // decomposition, so instead of emitting it twice, both scalars just point at
  // the same decomposition index.
  var uniqueDecomps: [[UInt32]: UInt16] = [:]
  
  for (scalar, _) in sortedData {
    let decomp = data.first { $0.scalar == scalar }!.decomposition

    // If we've seen this decomp before, use it.
    if let idx = uniqueDecomps[decomp] {
      indices.append((scalar, idx))
      continue
    }
    
    indices.append((scalar, UInt16(decompResult.count)))
    
    // This is our NFD decomposition utf8 string count.
    decompResult.append(0)
    let sizeIdx = decompResult.count - 1
    
    uniqueDecomps[decomp] = UInt16(sizeIdx)
    
    for scalar in decomp {
      let realScalar = Unicode.Scalar(scalar)!
      
      decompResult[sizeIdx] += UInt8(realScalar.utf8.count)
      
      for utf8 in realScalar.utf8 {
        decompResult.append(utf8)
      }
    }
  }
  
  result += """
  static const __swift_uint8_t \(kind.mappingTableBaseName)[\(decompResult.count)] = {

  """
  
  formatCollection(decompResult, into: &result) { value -> String in
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  result += "\n};\n\n"
  
  return indices
}

private func emitDecompIndices(
  _ indices: [(UInt32, UInt16)],
  kind: DecompositionKind,
  into result: inout String
) {
  result += """
  static const __swift_uint32_t \(kind.mappingTableBaseName)_indices[\(indices.count)] = {

  """
  
  formatCollection(indices, into: &result) { (scalar, idx) -> String in
    // Make sure that these scalars don't exceed past 18 bits. We need the other
    // 14 bits to store the index into decomp array. Although Unicode scalars
    // can go up to 21 bits, none of the higher scalars actually decompose into
    // anything or aren't assigned yet.
    precondition(scalar <= 0x3FFFF, "Scalar does not fit in 18 bits")
    precondition(idx <= 0x3FFF, "Decomposition index does not fit in 14 bits")
    var value = scalar
    value |= UInt32(idx) << 18
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  result += "\n};\n\n"
}
