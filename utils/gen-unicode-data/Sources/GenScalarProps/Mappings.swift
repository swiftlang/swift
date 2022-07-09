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

func getMappings(
  from data: String,
  into dict: inout [UInt32: (Int?, Int?, Int?)]
) {
  for line in data.split(separator: "\n") {
    let components = line.split(separator: ";", omittingEmptySubsequences: false)
    
    let uppercaseMapping = Int(components[12], radix: 16)
    let lowercaseMapping = Int(components[13], radix: 16)
    let titlecaseMapping = Int(components[14], radix: 16)
    
    guard uppercaseMapping != nil ||
          lowercaseMapping != nil ||
          titlecaseMapping != nil else {
      continue
    }
    
    let scalarStr = components[0]
    let scalar = UInt32(scalarStr, radix: 16)!
    
    dict[scalar] = (uppercaseMapping, lowercaseMapping, titlecaseMapping)
  }
}

func getSpecialMappings(
  from data: String,
  into dict: inout [UInt32: ([UInt32], [UInt32], [UInt32])]
) {
  for line in data.split(separator: "\n") {
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let components = line.split(separator: ";", omittingEmptySubsequences: false)
    
    // Conditional mappings have an extra component with the conditional name.
    // Ignore those.
    guard components.count == 5 else {
      continue
    }
    
    let scalar = UInt32(components[0], radix: 16)!
    
    let lowercaseMapping = components[1].split(separator: " ").map {
      UInt32($0, radix: 16)!
    }
    
    let titlecaseMapping = components[2].split(separator: " ").map {
      UInt32($0, radix: 16)!
    }
    
    let uppercaseMapping = components[3].split(separator: " ").map {
      UInt32($0, radix: 16)!
    }
    
    dict[scalar] = (uppercaseMapping, lowercaseMapping, titlecaseMapping)
  }
}

func emitMappings(
  _ data: [UInt32: (Int?, Int?, Int?)],
  into result: inout String
) {
  var uniqueDistances: Set<Int> = []

  for (scalar, mappings) in data {
    if let uppercaseMapping = mappings.0 {
      uniqueDistances.insert(uppercaseMapping - Int(scalar))
    }
    
    if let lowercaseMapping = mappings.1 {
      uniqueDistances.insert(lowercaseMapping - Int(scalar))
    }
    
    if let titlecaseMapping = mappings.2 {
      uniqueDistances.insert(titlecaseMapping - Int(scalar))
    }
  }
  
  let distances = Array(uniqueDistances)
  
  // 64 bit arrays * 8 bytes = .512 KB
  var bitArrays: [BitArray] = .init(repeating: .init(size: 64), count: 64)
  
  let chunkSize = 0x110000 / 64 / 64
  
  var chunks: [Int] = []
  
  for i in 0 ..< 64 * 64 {
    let lower = i * chunkSize
    let upper = lower + chunkSize - 1
    
    let idx = i / 64
    let bit = i % 64
    
    for scalar in lower ... upper {
      if data.contains(where: { $0.0 == scalar }) {
        chunks.append(i)
        
        bitArrays[idx][bit] = true
        break
      }
    }
  }
  
  // Remove the trailing 0s. Currently this reduces quick look size down to
  // 96 bytes from 512 bytes.
  var reducedBA = Array(bitArrays.reversed())
  reducedBA = Array(reducedBA.drop {
    $0.words == [0x0]
  })
  
  bitArrays = reducedBA.reversed()
  
  // Keep a record of every rank for all the bitarrays.
  var ranks: [UInt16] = []
  
  // Record our quick look ranks.
  var lastRank: UInt16 = 0
  for (i, _) in bitArrays.enumerated() {
    guard i != 0 else {
      ranks.append(0)
      continue
    }
    
    var rank = UInt16(bitArrays[i - 1].words[0].nonzeroBitCount)
    rank += lastRank
    
    ranks.append(rank)
    
    lastRank = rank
  }
  
  // Insert our quick look size at the beginning.
  var size = BitArray(size: 64)
  size.words = [UInt64(bitArrays.count)]
  bitArrays.insert(size, at: 0)
  
  var dataIndices: [UInt32] = []
  
  for chunk in chunks {
    var chunkBA = BitArray(size: chunkSize)
    
    let lower = chunk * chunkSize
    let upper = lower + chunkSize
    
    let chunkDataIdx = UInt64(dataIndices.endIndex)
    
    // Insert our chunk's data index in the upper bits of the last word of our
    // bit array.
    chunkBA.words[chunkBA.words.endIndex - 1] |= chunkDataIdx << 16
    
    for scalar in lower ..< upper {
      if data.contains(where: { $0.0 == scalar }) {
        chunkBA[scalar % chunkSize] = true
        
        let mappings = data[UInt32(scalar)]!
        
        var dataIdx: UInt32 = 0
        
        if let uppercaseMapping = mappings.0 {
          let distance = uppercaseMapping - scalar
          let uppercaseIdx = distances.firstIndex(of: distance)!
          
          dataIdx = UInt32(uppercaseIdx)
        } else {
          dataIdx = UInt32(UInt8.max)
        }
        
        if let lowercaseMapping = mappings.1 {
          let distance = lowercaseMapping - scalar
          let lowercaseIdx = distances.firstIndex(of: distance)!
          
          dataIdx |= UInt32(lowercaseIdx) << 8
        } else {
          dataIdx |= UInt32(UInt8.max) << 8
        }
        
        if let titlecaseMapping = mappings.2 {
          let distance = titlecaseMapping - scalar
          let titlecaseIdx = distances.firstIndex(of: distance)!
          
          dataIdx |= UInt32(titlecaseIdx) << 16
        } else {
          dataIdx |= UInt32(UInt8.max) << 16
        }
        
        dataIndices.append(dataIdx)
      }
    }
    
    // Append our chunk bit array's rank.
    var lastRank: UInt16 = 0
    for (i, _) in chunkBA.words.enumerated() {
      guard i != 0 else {
        ranks.append(0)
        continue
      }
      
      var rank = UInt16(chunkBA.words[i - 1].nonzeroBitCount)
      rank += lastRank
      
      ranks.append(rank)
      lastRank = rank
    }
    
    bitArrays += chunkBA.words.map {
      var ba = BitArray(size: 64)
      ba.words = [$0]
      return ba
    }
  }
  
  emitCollection(
    distances,
    name: "_swift_stdlib_mappings_data",
    type: "__swift_int32_t",
    into: &result
  ) {
    "\($0)"
  }
  
  emitCollection(
    dataIndices,
    name: "_swift_stdlib_mappings_data_indices",
    into: &result
  )
  
  emitCollection(
    ranks,
    name: "_swift_stdlib_mappings_ranks",
    into: &result
  )
  
  emitCollection(
    bitArrays,
    name: "_swift_stdlib_mappings",
    type: "__swift_uint64_t",
    into: &result
  ) {
    assert($0.words.count == 1)
    return "0x\(String($0.words[0], radix: 16, uppercase: true))"
  }
}

func emitSpecialMappings(
  _ data: [UInt32: ([UInt32], [UInt32], [UInt32])],
  into result: inout String
) {
  var specialMappings: [UInt8] = []
  var index: UInt32 = 0
  var scalarIndices: [UInt32: UInt32] = [:]
  
  for (scalar, (uppercase, lowercase, titlecase)) in data {
    scalarIndices[scalar] = index
    
    index += 1
    if uppercase.count == 1 {
      specialMappings.append(0)
    } else {
      let uppercase = uppercase.map { Unicode.Scalar($0)! }
      
      var utf8Length: UInt8 = 0
      
      for scalar in uppercase {
        utf8Length += UInt8(scalar.utf8.count)
      }
      
      specialMappings.append(utf8Length)
      
      for scalar in uppercase {
        for byte in String(scalar).utf8 {
          specialMappings.append(byte)
          index += 1
        }
      }
    }
    
    index += 1
    if lowercase.count == 1 {
      specialMappings.append(0)
    } else {
      let lowercase = lowercase.map { Unicode.Scalar($0)! }
      
      var utf8Length: UInt8 = 0
      
      for scalar in lowercase {
        utf8Length += UInt8(scalar.utf8.count)
      }
      
      specialMappings.append(utf8Length)
      
      for scalar in lowercase {
        for byte in String(scalar).utf8 {
          specialMappings.append(byte)
          index += 1
        }
      }
    }
    
    index += 1
    if titlecase.count == 1 {
      specialMappings.append(0)
    } else {
      let titlecase = titlecase.map { Unicode.Scalar($0)! }
      
      var utf8Length: UInt8 = 0
      
      for scalar in titlecase {
        utf8Length += UInt8(scalar.utf8.count)
      }
      
      specialMappings.append(utf8Length)
      
      for scalar in titlecase {
        for byte in String(scalar).utf8 {
          specialMappings.append(byte)
          index += 1
        }
      }
    }
  }
  
  // 64 bit arrays * 8 bytes = .512 KB
  var bitArrays: [BitArray] = .init(repeating: .init(size: 64), count: 64)
  
  let chunkSize = 0x110000 / 64 / 64
  
  var chunks: [Int] = []
  
  for i in 0 ..< 64 * 64 {
    let lower = i * chunkSize
    let upper = lower + chunkSize - 1
    
    let idx = i / 64
    let bit = i % 64
    
    for scalar in lower ... upper {
      if data.contains(where: { $0.0 == scalar }) {
        chunks.append(i)
        
        bitArrays[idx][bit] = true
        break
      }
    }
  }
  
  // Remove the trailing 0s. Currently this reduces quick look size down to
  // 96 bytes from 512 bytes.
  var reducedBA = Array(bitArrays.reversed())
  reducedBA = Array(reducedBA.drop {
    $0.words == [0x0]
  })
  
  bitArrays = reducedBA.reversed()
  
  // Keep a record of every rank for all the bitarrays.
  var ranks: [UInt16] = []
  
  // Record our quick look ranks.
  var lastRank: UInt16 = 0
  for (i, _) in bitArrays.enumerated() {
    guard i != 0 else {
      ranks.append(0)
      continue
    }
    
    var rank = UInt16(bitArrays[i - 1].words[0].nonzeroBitCount)
    rank += lastRank
    
    ranks.append(rank)
    
    lastRank = rank
  }
  
  // Insert our quick look size at the beginning.
  var size = BitArray(size: 64)
  size.words = [UInt64(bitArrays.count)]
  bitArrays.insert(size, at: 0)
  
  var dataIndices: [UInt16] = []
  
  for chunk in chunks {
    var chunkBA = BitArray(size: chunkSize)
    
    let lower = chunk * chunkSize
    let upper = lower + chunkSize
    
    let chunkDataIdx = UInt64(dataIndices.endIndex)
    
    // Insert our chunk's data index in the upper bits of the last word of our
    // bit array.
    chunkBA.words[chunkBA.words.endIndex - 1] |= chunkDataIdx << 16
    
    for scalar in lower ..< upper {
      if data.contains(where: { $0.0 == scalar }) {
        chunkBA[scalar % chunkSize] = true
        
        let dataIdx = UInt16(scalarIndices[UInt32(scalar)]!)
        dataIndices.append(dataIdx)
      }
    }
    
    // Append our chunk bit array's rank.
    var lastRank: UInt16 = 0
    for (i, _) in chunkBA.words.enumerated() {
      guard i != 0 else {
        ranks.append(0)
        continue
      }
      
      var rank = UInt16(chunkBA.words[i - 1].nonzeroBitCount)
      rank += lastRank
      
      ranks.append(rank)
      lastRank = rank
    }
    
    bitArrays += chunkBA.words.map {
      var ba = BitArray(size: 64)
      ba.words = [$0]
      return ba
    }
  }
  
  emitCollection(
    specialMappings,
    name: "_swift_stdlib_special_mappings_data",
    into: &result
  )
  
  emitCollection(
    dataIndices,
    name: "_swift_stdlib_special_mappings_data_indices",
    into: &result
  )
  
  emitCollection(
    ranks,
    name: "_swift_stdlib_special_mappings_ranks",
    into: &result
  )
  
  emitCollection(
    bitArrays,
    name: "_swift_stdlib_special_mappings",
    type: "__swift_uint64_t",
    into: &result
  ) {
    assert($0.words.count == 1)
    return "0x\(String($0.words[0], radix: 16, uppercase: true))"
  }
}

func generateMappingProps(for platform: String, into result: inout String) {
  let unicodeData: String
  
  switch platform {
  case "Apple":
    unicodeData = readFile("Data/Apple/UnicodeData.txt")
  default:
    unicodeData = readFile("Data/UnicodeData.txt")
  }
  
  let specialCasing = readFile("Data/SpecialCasing.txt")
  
  var data: [UInt32: (Int?, Int?, Int?)] = [:]
  getMappings(from: unicodeData, into: &data)
  emitMappings(data, into: &result)
  
  var specialMappings: [UInt32: ([UInt32], [UInt32], [UInt32])] = [:]
  getSpecialMappings(from: specialCasing, into: &specialMappings)
  emitSpecialMappings(specialMappings, into: &result)
}
