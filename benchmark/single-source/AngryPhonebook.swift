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


// Workloads for various scripts. Always 20 names for 400 pairings.
// To keep the performance of various scripts roughly comparable, aim for
// a total length of approximately 120 characters.
// E.g.: `ascii.joined(separator: "").count == 124`
// Every name should fit in 15-bytes UTF-8 encoded, to excercise the small
// string optimization.
// E.g.: `armenian.allSatisfy { $0._guts.isSmall } == true`

// Workload Size Statistics
//   SMALL  | UTF-8 | UTF-16 |    REGULAR   |  UTF-8  | UTF-16
// ---------|-------|--------|--------------|---------|--------
//    ascii | 124 B |  248 B |    longASCII |  6158 B | 12316 B
//  strasse | 140 B |  240 B |  longStrasse |  6798 B | 11996 B
// armenian | 232 B |  232 B | longArmenian | 10478 B | 11676 B
// cyrillic | 238 B |  238 B | longCyrillic | 10718 B | 11916 B

let words = [
  "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph",
  "Charles", "Thomas", "Christopher", "Daniel", "Matthew", "Donald", "Anthony",
  "Paul", "Mark", "George", "Steven", "Kenneth", "Andrew", "Edward", "Brian",
  "Joshua", "Kevin", "Ronald", "Timothy", "Jason", "Jeffrey", "Gary", "Ryan",
  "Nicholas", "Eric", "Stephen", "Jacob", "Larry", "Frank"]

let ascii = Array(words.prefix(20))
// Pathological case, uppercase: ß -> SS
let strasse = Array(repeating: "Straße", count: 20)

let armenian = [
  "Արմեն", "Աննա", "Հարութ", "Միքայել", "Մարիա", "Դավիթ", "Վարդան",
  "Նարինե", "Տիգրան", "Տաթևիկ", "Թագուհի", "Թամարա", "Ազնաուր", "Գրիգոր",
  "Կոմիտաս", "Հայկ", "Գառնիկ", "Վահրամ", "Վահագն", "Գևորգ"]

let cyrillic = [
  "Ульяна", "Аркадий", "Аня", "Даниил", "Дмитрий", "Эдуард", "Юрій", "Давид",
  "Анна", "Дмитрий", "Евгений", "Борис", "Ксения", "Артур", "Аполлон",
  "Соломон", "Николай", "Кристи", "Надежда", "Спартак"]

/// Precompose the phonebook into one large string of comma separated names.
func phonebook(_ names: [String]) -> String {
  names.map { firstName in
    names.map { lastName in
      firstName + " " + lastName
    }.joined(separator: ", ")
  }.joined(separator: ", ")
}

let longASCII = phonebook(ascii)
let longStrasse = phonebook(strasse)
let longArmenian = phonebook(armenian)
let longCyrillic = phonebook(cyrillic)


struct LegacyAngryPhonebook: Benchmark {
  let name: String

  init(name: String) {
    self.name = name
  }

  var tags: Tags { [.validation, .api, .String] }
  var legacyFactor: Int? { 7 }

  func run(iterations: Int) {
    run_AngryPhonebook(iterations)
  }

  @inline(never)
  @usableFromInline
  func run_AngryPhonebook(_ iterations: Int) {
    // Permute the names.
    for _ in 1...iterations {
      for firstname in words {
        for lastname in words {
          _ = (firstname.uppercased(), lastname.lowercased())
        }
      }
    }
  }
}

struct AngryPhonebook: Benchmark {
  let name: String
  let input: [String]
  let factor: Int

  init(name: String, input: [String], factor: Int = 1) {
    self.name = name
    self.input = input
    self.factor = factor
  }

  var tags: Tags { [.validation, .api, .String] }

  func run(iterations: Int) {
    Self.angryPhonebook(factor * iterations, input)
  }

  @inline(never)
  @usableFromInline
  static func angryPhonebook(_ iterations: Int, _ names: [String]) {
    assert(names.count == 20)
    // Permute the names.
    for _ in 1...iterations {
      for firstname in names {
        for lastname in names {
          blackHole((firstname.uppercased(), lastname.lowercased()))
        }
      }
    }
  }
}

struct PrecomposedAngryPhonebook: Benchmark {
  let name: String
  let input: String
  let factor: Int

  init(name: String, input: String, factor: Int = 1) {
    self.name = name
    self.input = input
    self.factor = factor
  }

  var tags: Tags { [.validation, .api, .String] }

  func run(iterations: Int) {
    Self.angryPhonebook(factor * iterations, input)
  }

  @inline(never)
  @usableFromInline
  static func angryPhonebook(_ iterations: Int, _ names: String) {
    for _ in 1...iterations {
      blackHole((names.uppercased(), names.lowercased()))
    }
  }
}

public let benchmarks: [Benchmark] = [
  LegacyAngryPhonebook(name: "AngryPhonebook"),

  // Small String Workloads
  AngryPhonebook(
    name: "AngryPhonebook.ASCII2.Small",
    input: ascii,
    factor: 10),
  AngryPhonebook(
    name: "AngryPhonebook.Strasse.Small",
    input: strasse),
  AngryPhonebook(
    name: "AngryPhonebook.Armenian.Small",
    input: armenian),
  AngryPhonebook(
    name: "AngryPhonebook.Cyrillic.Small",
    input: cyrillic),

  // Regular String Workloads
  PrecomposedAngryPhonebook(
    name: "AngryPhonebook.ASCII2",
    input: longASCII,
    factor: 10),
  PrecomposedAngryPhonebook(
    name: "AngryPhonebook.Strasse",
    input: longStrasse),
  PrecomposedAngryPhonebook(
    name: "AngryPhonebook.Armenian",
    input: longArmenian),
  PrecomposedAngryPhonebook(
    name: "AngryPhonebook.Cyrillic",
    input: longCyrillic),
]

