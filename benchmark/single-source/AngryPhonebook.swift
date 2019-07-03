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

let t: [BenchmarkCategory] = [.validation, .api, .String]

public let AngryPhonebook = [
  BenchmarkInfo(
    name: "AngryPhonebook",
    runFunction: run_AngryPhonebook,
    tags: t,
    legacyFactor: 7),

  // Small String Workloads
  BenchmarkInfo(
    name: "AngryPhonebook.Latin",
    runFunction: { angryPhonebook($0, latin) },
    tags: t,
    setUpFunction: { blackHole(latin) }),
  BenchmarkInfo(
    name: "AngryPhonebook.Armenian",
    runFunction: { angryPhonebook($0, armenian) },
    tags: t,
    setUpFunction: { blackHole(armenian) }),
  BenchmarkInfo(
    name: "AngryPhonebook.Cyrillic",
    runFunction: { angryPhonebook($0, cyrillic) },
    tags: t,
    setUpFunction: { blackHole(cyrillic) }),

  // Large String Workloads
  BenchmarkInfo(
    name: "AngryPhonebook.Latin.Large",
    runFunction: { largeAngryPhonebook($0, latinLarge) },
    tags: t,
    setUpFunction: { blackHole(latinLarge) }),
  BenchmarkInfo(
    name: "AngryPhonebook.Armenian.Large",
    runFunction: { largeAngryPhonebook($0, armenianLarge) },
    tags: t,
    setUpFunction: { blackHole(armenianLarge) }),
  BenchmarkInfo(
    name: "AngryPhonebook.Cyrillic.Large",
    runFunction: { largeAngryPhonebook($0, cyrillicLarge) },
    tags: t,
    setUpFunction: { blackHole(cyrillicLarge) })
]

let words = [
  "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph",
  "Charles", "Thomas", "Christopher", "Daniel", "Matthew", "Donald", "Anthony",
  "Paul", "Mark", "George", "Steven", "Kenneth", "Andrew", "Edward", "Brian",
  "Joshua", "Kevin", "Ronald", "Timothy", "Jason", "Jeffrey", "Gary", "Ryan",
  "Nicholas", "Eric", "Stephen", "Jacob", "Larry", "Frank"]

@inline(never)
public func run_AngryPhonebook(_ N: Int) {
  // Permute the names.
  for _ in 1...N {
    for firstname in words {
      for lastname in words {
        _ = (firstname.uppercased(), lastname.lowercased())
      }
    }
  }
}

// Workloads for various scripts. Always 20 names for 400 pairings.
// To keep the performance of various scripts roughly comparable, aim for
// a total length of approximately 120 characters.
// E.g.: `latin.joined(separator: "").count == 124`

/// Precompose the phonebook into one large string of comma separated names.
func large(_ names: [String]) -> String {
  names.map { firstName in
    names.map { lastName in
      firstName + " " + lastName }
      .joined(separator: ", ")
    }.joined(separator: ", ")
}

let latin = Array(words.prefix(20))
let latinLarge = large(latin)

let armenian = [
  "Արմեն", "Աննա", "Հարութ", "Միքայել", "Մարիա", "Դավիթ", "Վարդան",
  "Նարինե", "Տիգրան", "Տաթևիկ", "Թագուհի", "Թամարա", "Ազնաուր", "Գրիգոր",
  "Կոմիտաս", "Հայկ", "Գառնիկ", "Վահրամ", "Վահագն", "Գևորգ"]
let armenianLarge = large(armenian)

let cyrillic = [
  "Ульяна", "Аркадий", "Аня", "Даниил", "Дмитрий", "Эдуард", "Юрій", "Давид",
  "Анна", "Дмитрий", "Евгений", "Борис", "Ксения", "Артур", "Аполлон",
  "Соломон", "Николай", "Кристи", "Надежда", "Спартак"]
let cyrillicLarge = large(cyrillic)

@inline(never)
public func angryPhonebook(_ N: Int, _ names: [String]) {
  assert(names.count == 20)
  // Permute the names.
  for _ in 1...N {
    for firstname in names {
      for lastname in names {
        blackHole((firstname.uppercased(), lastname.lowercased()))
      }
    }
  }
}

@inline(never)
public func largeAngryPhonebook(_ N: Int, _ names: String) {
  for _ in 1...N {
    blackHole((names.uppercased(), names.lowercased()))
  }
}
