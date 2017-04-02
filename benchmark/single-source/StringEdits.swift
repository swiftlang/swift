//===--- StringEdits.swift-------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils
#if os(Linux)
import Glibc
#else
import Darwin
#endif

var editWords: [String] = [
  "woodshed",
  "lakism",
  "gastroperiodynia",
]

let alphabet = "abcdefghijklmnopqrstuvwxyz"
/// All edits that are one edit away from `word`
func edits(_ word: String) -> Set<String> {
  // create right/left splits as CharacterViews instead
  let splits = word.indices.map {
    (word.characters[word.characters.startIndex..<$0],word.characters[$0..<word.characters.endIndex])
  }
  
  // though it should be, CharacterView isn't hashable
  // so using an array for now, ignore that aspect...
  var result: Set<String> = []
  
  for (left,right) in splits {
    // drop a character
    result.insert(String(left + right.dropFirst()))
    
    // transpose two characters
    if let fst = right.first {
      let drop1 = right.dropFirst()
      if let snd = drop1.first {
        var str = ""
        str += left
        str.append(snd)
        str.append(fst)
        str += drop1.dropFirst()
        result.insert(str)
      }
    }
    
    // replace each character with another
    for letter in alphabet {
      var str = ""
      str += left
      str.append(letter)
      str += right.dropFirst()
      result.insert(str)
    }
    
    // insert rogue characters
    for letter in alphabet {
      var str = ""
      str += left
      str.append(letter)
      str += right
      result.insert(str)
    }
  }
  
  // have to map back to strings right at the end
  return result
}

@inline(never)
public func run_StringEdits(_ N: Int) {
  for _ in 1...N*100 {
    for word in editWords {
      _ = edits(word)      
    }
  }
}

