//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import GenUtils

func getEmojiData(
  for path: String
) -> [ClosedRange<UInt32>] {
  let data = readFile(path)

  var unflattened: [ClosedRange<UInt32>] = []

  for line in data.split(separator: "\n") {
    // Skip comments
    guard !line.hasPrefix("#") else {
      continue
    }

    // Each line in this file is broken up into two sections:
    // 1: Either the singular scalar or a range of scalars who conform to said
    //    grapheme break property.
    // 2: The grapheme break property that said scalar(s) conform to (with
    //    additional comments noting the character category, name and amount of
    //    scalars the range represents).
    let components = line.split(separator: ";")

    // Get the property first because it may be one we don't care about.
    let splitProperty = components[1].split(separator: "#")
    let filteredProperty = splitProperty[0].filter { !$0.isWhitespace }

    guard filteredProperty == "Extended_Pictographic" else {
      continue
    }

    let scalars: ClosedRange<UInt32>

    let filteredScalars = components[0].filter { !$0.isWhitespace }

    // If we have . appear, it means we have a legitimate range. Otherwise,
    // it's a singular scalar.
    if filteredScalars.contains(".") {
      let range = filteredScalars.split(separator: ".")

      scalars = UInt32(range[0], radix: 16)! ... UInt32(range[1], radix: 16)!
    } else {
      let scalar = UInt32(filteredScalars, radix: 16)!

      scalars = scalar ... scalar
    }

    unflattened.append(scalars)
  }

  return flatten(unflattened)
}

func emitEmojiData(
  _ data: [ClosedRange<UInt32>],
  into result: inout String
) {
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
      if data.contains(where: { $0.contains(UInt32(scalar)) }) {
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

  for chunk in chunks {
    var chunkBA = BitArray(size: chunkSize)

    let lower = chunk * chunkSize
    let upper = lower + chunkSize

    let chunkDataIdx = UInt64(dataIndices.endIndex)

    // Insert our chunk's data index in the upper bits of the last word of our
    // bit array.
    chunkBA.words[chunkBA.words.endIndex - 1] |= chunkDataIdx << 16

    for scalar in lower ..< upper {
      if data.contains(where: { $0.contains(UInt32(scalar)) }) {
        chunkBA[scalar % chunkSize] = true
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
    ranks,
    name: "_swift_stdlib_emojiData_ranks",
    into: &result
  )

  emitCollection(
    bitArrays,
    name: "_swift_stdlib_emojiData",
    type: "__swift_uint64_t",
    into: &result
  ) {
    assert($0.words.count == 1)
    return "0x\(String($0.words[0], radix: 16, uppercase: true))"
  }
}

// Main entry point into the word break generator.
func generateEmoji() {
  var result = readFile("Input/EmojiData.h")

  let emojiData = getEmojiData(for: "Data/17/emoji-data.txt")

  emitEmojiData(emojiData, into: &result)

  result += """
  #endif // #ifndef EMOJI_DATA_H

  """

  write(result, to: "Output/Common/EmojiData.h")
}

generateEmoji()
