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

func getName(
  from data: String,
  into result: inout [(UInt32, String)],
  words: inout [String]
) {
  var uniqueWords: Set<String> = []
  
  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }
    
    let info = line.split(separator: "#")
    let components = info[0].split(separator: ";")
    
    let name = String(components[1].dropFirst())
    
    let filteredScalars = components[0].filter { !$0.isWhitespace }
    
    if filteredScalars.contains(".") {
      continue
    }
    
    let scalar = UInt32(filteredScalars, radix: 16)!
    
    if (0xE0100...0xE01EF).contains(scalar) {
      continue
    }
    
    if scalar >= 0xAC00, scalar <= 0xD7A3 {
      continue
    }
    
    result.append((scalar, name))
    
    for word in name.split(separator: " ") {
      uniqueWords.insert(String(word))
    }
  }
  
  words = Array(uniqueWords)
}

func sortWords(
  _ words: inout [String],
  from data: [(UInt32, String)]
) {
  var popularity: [String: Int] = [:]
  
  for (_, name) in data {
    let scalarWords = name.split(separator: " ")
    
    for word in scalarWords {
      popularity[String(word), default: 0] += 1
    }
  }
  
  let sortedPopularity = Array(popularity).sorted { $0.value > $1.value }
  
  
  words = sortedPopularity.map { $0.key }
}

func emitWords(
  _ words: [String],
  into result: inout String
) -> [String: UInt32] {
  var wordIndices: [String: UInt32] = [:]
  var bytes: [UInt8] = []
  
  var index: UInt32 = 0
  
  for word in words {
    wordIndices[word] = index
    
    for (i, byte) in word.utf8.enumerated() {
      var element = byte
      
      if i == word.utf8.count - 1 {
        element |= 0x80
      }
      
      bytes.append(element)
      index += 1
    }
  }
  
  emitCollection(bytes, name: "_swift_stdlib_words", into: &result)
  
  return wordIndices
}

func emitWordOffsets(
  _ wordOffsets: [String: UInt32],
  into result: inout String
) -> [String: UInt32] {
  let sortedWordOffsets = Array(wordOffsets).sorted { $0.value < $1.value }
  
  var wordIndices: [String: UInt32] = [:]
  
  for (i, (word, _)) in sortedWordOffsets.enumerated() {
    wordIndices[word] = UInt32(i)
  }
  
  emitCollection(
    sortedWordOffsets.map { $0.value },
    name: "_swift_stdlib_word_indices",
    into: &result
  )
  
  return wordIndices
}

func emitScalarNames(
  _ names: [(UInt32, String)],
  _ wordIndices: [String: UInt32],
  into result: inout String
) -> [UInt32: UInt32] {
  var nameBytes: [UInt8] = []
  
  var scalarNameIndices: [UInt32: UInt32] = [:]
  
  var index: UInt32 = 0
  
  for (scalar, name) in names.sorted(by: { $0.0 < $1.0 }) {
    scalarNameIndices[scalar] = index
    
    for word in name.split(separator: " ") {
      let wordIndex = wordIndices[String(word)]!
      
      // If the word index is smaller than 0xFF, then we don't need to add the
      // extra byte to represent the index.
      if wordIndex < 0xFF {
        nameBytes.append(UInt8(wordIndex))
        index += 1
      } else {
        assert(wordIndex <= UInt16.max)
        
        nameBytes.append(0xFF)
        nameBytes.append(UInt8(wordIndex & 0xFF))
        nameBytes.append(UInt8(wordIndex >> 8))
        index += 3
      }
    }
  }
  
  result += """
  #define NAMES_LAST_SCALAR_OFFSET \(nameBytes.count)
  
  
  """
  
  emitCollection(nameBytes, name: "_swift_stdlib_names", into: &result)
  
  return scalarNameIndices
}

func emitScalars(
  _ scalarNameIndices: [UInt32: UInt32],
  into result: inout String
) -> [UInt32: UInt16] {
  var scalars: [UInt32] = []
  var scalarSetIndices: [UInt32: UInt16] = [:]
  var index: UInt16 = 0
  
  for i in 0x0 ... 0x10FFFF >> 7 {
    let scalarRange = i << 7 ..< i << 7 + 128
    let filteredRange = scalarRange.filter {
      scalarNameIndices.keys.contains(UInt32($0))
    }
    
    scalarSetIndices[UInt32(i)] = index
    
    if filteredRange.count >= 1 {
      scalarSetIndices[UInt32(i)] = index
      
      for scalar in scalarRange {
        index += 1
        
        guard let index = scalarNameIndices[UInt32(scalar)] else {
          scalars.append(0)
          continue
        }
        
        scalars.append(index)
      }
    } else {
      scalarSetIndices[UInt32(i)] = UInt16.max
    }
  }
  
  result += """
  #define NAMES_SCALARS_MAX_INDEX \(scalars.count - 1)
  
  
  """
  
  emitCollection(scalars, name: "_swift_stdlib_names_scalars", into: &result)
  
  return scalarSetIndices
}

func emitScalarSets(
  _ scalarSetIndices: [UInt32: UInt16],
  into result: inout String
) {
  var scalarSets: [UInt16] = []
  
  for i in 0x0 ... 0x10FFFF >> 7 {
    let index = scalarSetIndices[UInt32(i)]!
    
    guard index != .max else {
      scalarSets.append(index)
      continue
    }
    
    scalarSets.append(index >> 7)
  }
  
  emitCollection(scalarSets, name: "_swift_stdlib_names_scalar_sets", into: &result)
}

func emitLargestNameCount(_ names: [(UInt32, String)], into result: inout String) {
  var largestCount = 0
  
  for (_, name) in names {
    largestCount = Swift.max(largestCount, name.count)
  }
  
  print("""
  Please copy and paste the following into 'stdlib/public/SwiftShims/UnicodeData.h':
  
  #define SWIFT_STDLIB_LARGEST_NAME_COUNT \(largestCount)
  
  """)
}

func generateNameProp(into result: inout String) {
  let derivedName = readFile("Data/DerivedName.txt")
  
  var names: [(UInt32, String)] = []
  var words: [String] = []
  
  getName(from: derivedName, into: &names, words: &words)
  
  sortWords(&words, from: names)
  
  emitLargestNameCount(names, into: &result)
  
  let wordOffsets = emitWords(words, into: &result)
  let wordIndices = emitWordOffsets(wordOffsets, into: &result)
  
  let scalarNameIndices = emitScalarNames(names, wordIndices, into: &result)
  let scalarSetIndices = emitScalars(scalarNameIndices, into: &result)
  emitScalarSets(scalarSetIndices, into: &result)
}
