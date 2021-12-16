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

// Given a string to the UnicodeData file, return the flattened list of scalar
// to Canonical Decompositions.
//
// Each line in this data file is formatted like the following:
//
//     1B06;BALINESE LETTER AKARA TEDUNG;Lo;0;L;1B05 1B35;;;;N;;;;;
//
// Where each section is split by a ';'. The first section informs us of the
// scalar in the line with the various properties. For the purposes of
// decomposition data, we only need the 1B05 1B35 after the L (index 5) which is
// the array of scalars that the scalars decomposes to.
func getDecompData(
  from data: String
) -> [(UInt32, [UInt32])] {
  var unflattened: [(UInt32, [UInt32])] = []
  
  for line in data.split(separator: "\n") {
    let components = line.split(separator: ";", omittingEmptySubsequences: false)
    
    let decomp = components[5]
    
    // We either 1. don't have decompositions, or 2. the decompositions is for
    // compatibile forms. We only care about NFD, so ignore these cases.
    if decomp == "" || decomp.hasPrefix("<") {
      continue
    }
    
    let decomposedScalars = decomp.split(separator: " ").map {
      UInt32($0, radix: 16)!
    }
    
    let scalarStr = components[0]
    let scalar = UInt32(scalarStr, radix: 16)!
    
    unflattened.append((scalar, decomposedScalars))
  }
  
  return unflattened
}

// Takes a mph for the keys and the data values and writes the required data into
// static C arrays.
func emitDecomp(
  _ mph: Mph,
  _ data: [(UInt32, [UInt32])],
  into result: inout String
) {
  emitMph(
    mph,
    name: "_swift_stdlib_nfd_decomp",
    defineLabel: "NFD_DECOMP",
    into: &result
  )
  
  // Fixup the decomposed scalars first for fully decompositions.
  
  var data = data
  
  func decompose(_ scalar: UInt32, into result: inout [UInt32]) {
    if scalar <= 0x7F {
      result.append(scalar)
      return
    }
    
    if let decomp = data.first(where: { $0.0 == scalar }) {
      for scalar in decomp.1 {
        decompose(scalar, into: &result)
      }
    } else {
      result.append(scalar)
    }
  }
  
  for (i, (_, rawDecomposed)) in data.enumerated() {
    var newDecomposed: [UInt32] = []
    
    for rawScalar in rawDecomposed {
      decompose(rawScalar, into: &newDecomposed)
    }
    
    data[i].1 = newDecomposed
  }
  
  var sortedData: [(UInt32, UInt16)] = []
  
  for (scalar, _) in data {
    sortedData.append((scalar, UInt16(mph.index(for: UInt64(scalar)))))
  }
  
  sortedData.sort { $0.1 < $1.1 }
  
  let indices = emitDecompDecomp(data, sortedData, into: &result)
  emitDecompIndices(indices, into: &result)
}

func emitDecompDecomp(
  _ data: [(UInt32, [UInt32])],
  _ sortedData: [(UInt32, UInt16)],
  into result: inout String
) -> [(UInt32, UInt16)] {
  var indices: [(UInt32, UInt16)] = []
  var decompResult: [UInt8] = []
  
  // Keep a record of decompositions because some scalars share the same
  // decomposition, so instead of emitting it twice, both scalars just point at
  // the same decomposition index.
  var uniqueDecomps: [[UInt32]: UInt16] = [:]
  
  for (scalar, _) in sortedData {
    let decomp = data.first(where: { $0.0 == scalar })!.1
    
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
  static const __swift_uint8_t _swift_stdlib_nfd_decomp[\(decompResult.count)] = {

  """
  
  formatCollection(decompResult, into: &result) { value -> String in
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  result += "\n};\n\n"
  
  return indices
}

func emitDecompIndices(
  _ indices: [(UInt32, UInt16)],
  into result: inout String
) {
  result += """
  static const __swift_uint32_t _swift_stdlib_nfd_decomp_indices[\(indices.count)] = {

  """
  
  formatCollection(indices, into: &result) { (scalar, idx) -> String in
    // Make sure that these scalars don't exceed past 18 bits. We need the other
    // 14 bits to store the index into decomp array. Although Unicode scalars
    // can go up to 21 bits, none of the higher scalars actually decompose into
    // anything or aren't assigned yet.
    assert(scalar <= 0x3FFFF)
    var value = scalar
    value |= UInt32(idx) << 18
    
    return "0x\(String(value, radix: 16, uppercase: true))"
  }
  
  result += "\n};\n\n"
}
