//===--- AngryPhonebook.swift ---------------------------------------------===//
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

// This test is based on single-source/Phonebook, with
// to test uppercase and lowercase ASCII string fast paths.
import TestsUtils
import Foundation

public let AngryPhonebook = BenchmarkInfo(
  name: "AngryPhonebook",
  runFunction: run_AngryPhonebook,
  tags: [.validation, .api, .String],
  legacyFactor: 7)

let words = [
  "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph",
  "Charles", "Thomas", "Christopher", "Daniel", "Matthew", "Donald", "Anthony",
  "Paul", "Mark", "George", "Steven", "Kenneth", "Andrew", "Edward", "Brian",
  "Joshua", "Kevin", "Ronald", "Timothy", "Jason", "Jeffrey", "Gary", "Ryan",
  "Nicholas", "Eric", "Stephen", "Jacob", "Larry", "Frank"]

let emojis = [
  "🏄", "🍔", "🎽", "🐟", "😔", "🚓", "🚼", "🍣", "🏃", "🚲", "😼", "🎷", "🙇", "👩", "🙉", "🍲", "😨", "🚃", "🎾", "🚫", "🐰", "😖", "🍫", "💄", "👹", "🚏", "🍢", "🎭", "😌", "🚍", "🍕", "🙈", "👧"]


@inline(never)
public func run_AngryPhonebook(_ N: Int) {
  // Permute the names.
  for _ in 1...N {
    for firstname in words {
      for lastname in words {
        blackHole((firstname.uppercased(), lastname.lowercased()))
      }
    }
  }
}
