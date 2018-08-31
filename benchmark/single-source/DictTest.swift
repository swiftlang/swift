//===--- DictTest.swift ---------------------------------------------------===//
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


public let Dictionary = [
  BenchmarkInfo(name: "Dictionary", runFunction: run_Dictionary, tags: [.validation, .api, .Dictionary]),
  BenchmarkInfo(name: "DictionaryOfObjects", runFunction: run_DictionaryOfObjects, tags: [.validation, .api, .Dictionary]),
]

@inline(never)
public func run_Dictionary(scale: Int) {
  let Input = [
    // Text from http://en.wikipedia.org/wiki/Hash_table
    "hash", "table",
      "in", "computing", "a", "hash", "table", "also", "hash", "map", "is",
      "a", "data", "structure", "used", "to", "implement", "an", "associative",
      "array", "a", "structure", "that", "can", "map", "keys", "to", "values",
      "a", "hash", "table", "uses", "a", "hash", "function", "to", "compute",
      "an", "index", "into", "an", "array", "of", "buckets", "or", "slots",
      "from", "which", "the", "correct", "value", "can", "be", "found",
      "ideally", "the", "hash", "function", "will", "assign", "each", "key",
      "to", "a", "unique", "bucket", "but", "this", "situation", "is",
      "rarely", "achievable", "in", "practice", "usually", "some", "keys",
      "will", "hash", "to", "the", "same", "bucket", "instead", "most", "hash",
      "table", "designs", "assume", "that", "hash", "collisions", "different",
      "keys", "that", "are", "assigned", "by", "the", "hash", "function", "to",
      "the", "same", "bucket", "will", "occur", "and", "must", "be",
      "accommodated", "in", "some", "way", "in", "a", "well", "dimensioned",
      "hash", "table", "the", "average", "cost", "number", "of",
      "instructions", "for", "each", "lookup", "is", "independent", "of",
      "the", "number", "of", "elements", "stored", "in", "the", "table",
      "many", "hash", "table", "designs", "also", "allow", "arbitrary",
      "insertions", "and", "deletions", "of", "key", "value", "pairs", "at",
      "amortized", "constant", "average", "cost", "per", "operation", "in",
      "many", "situations", "hash", "tables", "turn", "out", "to", "be",
      "more", "efficient", "than", "search", "trees", "or", "any", "other",
      "table", "lookup", "structure", "for", "this", "reason", "they", "are",
      "widely", "used", "in", "many", "kinds", "of", "computer", "software",
      "particularly", "for", "associative", "arrays", "database", "indexing",
      "caches", "and", "sets",

    "hashing",
      "the", "idea", "of", "hashing", "is", "to", "distribute", "the",
      "entries", "key", "value", "pairs", "across", "an", "array", "of",
      "buckets", "given", "a", "key", "the", "algorithm", "computes", "an",
      "index", "that", "suggests", "where", "the", "entry", "can", "be",
      "found", "index", "f", "key", "array", "size", "often", "this", "is",
      "done", "in", "two", "steps", "hash", "hashfunc", "key", "index", "hash",
      "array", "size", "in", "this", "method", "the", "hash", "is",
      "independent", "of", "the", "array", "size", "and", "it", "is", "then",
      "reduced", "to", "an", "index", "a", "number", "between", "and", "array",
      "size", "using", "the", "modulus", "operator", "in", "the", "case",
      "that", "the", "array", "size", "is", "a", "power", "of", "two", "the",
      "remainder", "operation", "is", "reduced", "to", "masking", "which",
      "improves", "speed", "but", "can", "increase", "problems", "with", "a",
      "poor", "hash", "function",

    "choosing", "a", "good", "hash", "function",
      "a", "good", "hash", "function", "and", "implementation", "algorithm",
      "are", "essential", "for", "good", "hash", "table", "performance", "but",
      "may", "be", "difficult", "to", "achieve", "a", "basic", "requirement",
      "is", "that", "the", "function", "should", "provide", "a", "uniform",
      "distribution", "of", "hash", "values", "a", "non", "uniform",
      "distribution", "increases", "the", "number", "of", "collisions", "and",
      "the", "cost", "of", "resolving", "them", "uniformity", "is",
      "sometimes", "difficult", "to", "ensure", "by", "design", "but", "may",
      "be", "evaluated", "empirically", "using", "statistical", "tests", "e",
      "g", "a", "pearson", "s", "chi", "squared", "test", "for", "discrete",
      "uniform", "distributions", "the", "distribution", "needs", "to", "be",
      "uniform", "only", "for", "table", "sizes", "that", "occur", "in", "the",
      "application", "in", "particular", "if", "one", "uses", "dynamic",
      "resizing", "with", "exact", "doubling", "and", "halving", "of", "the",
      "table", "size", "s", "then", "the", "hash", "function", "needs", "to",
      "be", "uniform", "only", "when", "s", "is", "a", "power", "of", "two",
      "on", "the", "other", "hand", "some", "hashing", "algorithms", "provide",
      "uniform", "hashes", "only", "when", "s", "is", "a", "prime", "number",
      "for", "open", "addressing", "schemes", "the", "hash", "function",
      "should", "also", "avoid", "clustering", "the", "mapping", "of", "two",
      "or", "more", "keys", "to", "consecutive", "slots", "such", "clustering",
      "may", "cause", "the", "lookup", "cost", "to", "skyrocket", "even", "if",
      "the", "load", "factor", "is", "low", "and", "collisions", "are",
      "infrequent", "the", "popular", "multiplicative", "hash", "3", "is",
      "claimed", "to", "have", "particularly", "poor", "clustering",
      "behavior", "cryptographic", "hash", "functions", "are", "believed",
      "to", "provide", "good", "hash", "functions", "for", "any", "table",
      "size", "s", "either", "by", "modulo", "reduction", "or", "by", "bit",
      "masking", "they", "may", "also", "be", "appropriate", "if", "there",
      "is", "a", "risk", "of", "malicious", "users", "trying", "to",
      "sabotage", "a", "network", "service", "by", "submitting", "requests",
      "designed", "to", "generate", "a", "large", "number", "of", "collisions",
      "in", "the", "server", "s", "hash", "tables", "however", "the", "risk",
      "of", "sabotage", "can", "also", "be", "avoided", "by", "cheaper",
      "methods", "such", "as", "applying", "a", "secret", "salt", "to", "the",
      "data", "or", "using", "a", "universal", "hash", "function",

    "perfect", "hash", "function",
      "if", "all", "keys", "are", "known", "ahead", "of", "time", "a",
      "perfect", "hash", "function", "can", "be", "used", "to", "create", "a",
      "perfect", "hash", "table", "that", "has", "no", "collisions", "if",
      "minimal", "perfect", "hashing", "is", "used", "every", "location", "in",
      "the", "hash", "table", "can", "be", "used", "as", "well", "perfect",
      "hashing", "allows", "for", "constant", "time", "lookups", "in", "the",
      "worst", "case", "this", "is", "in", "contrast", "to", "most",
      "chaining", "and", "open", "addressing", "methods", "where", "the",
      "time", "for", "lookup", "is", "low", "on", "average", "but", "may",
      "be", "very", "large", "proportional", "to", "the", "number", "of",
      "entries", "for", "some", "sets", "of", "keys"
  ]

  var Dict: Dictionary<String, Bool> = [:]
  let N = 5*scale

  // Check performance of filling the dictionary:
  for _ in 1...N {
    Dict = [:]
    for word in Input {
      Dict[word] = true
    }
  }
  CheckResults(Dict.count == 270)

  // Check performance of searching in the dictionary:
  // Fill the dictionary with words from the first half of the text
  Dict = [:]
  for i in 0 ..< Input.count/2 {
    let word = Input[i]
    Dict[word] = true
  }

  // Count number of words from the first half in the entire text
  var count = 0
  for _ in 1...N {
    for word in Input {
      if Dict[word] != nil {
        count += 1
      }
    }
  }
  CheckResults(count == N*541)
}

class Box<T : Hashable> : Hashable {
  var value: T

  init(_ v: T) {
    value = v
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }

  static func ==(lhs: Box, rhs: Box) -> Bool {
    return lhs.value == rhs.value
  }
}

@inline(never)
public func run_DictionaryOfObjects(scale: Int) {
  let Input = [
    // Text from http://en.wikipedia.org/wiki/Hash_table
    "hash", "table",
      "in", "computing", "a", "hash", "table", "also", "hash", "map", "is",
      "a", "data", "structure", "used", "to", "implement", "an", "associative",
      "array", "a", "structure", "that", "can", "map", "keys", "to", "values",
      "a", "hash", "table", "uses", "a", "hash", "function", "to", "compute",
      "an", "index", "into", "an", "array", "of", "buckets", "or", "slots",
      "from", "which", "the", "correct", "value", "can", "be", "found",
      "ideally", "the", "hash", "function", "will", "assign", "each", "key",
      "to", "a", "unique", "bucket", "but", "this", "situation", "is",
      "rarely", "achievable", "in", "practice", "usually", "some", "keys",
      "will", "hash", "to", "the", "same", "bucket", "instead", "most", "hash",
      "table", "designs", "assume", "that", "hash", "collisions", "different",
      "keys", "that", "are", "assigned", "by", "the", "hash", "function", "to",
      "the", "same", "bucket", "will", "occur", "and", "must", "be",
      "accommodated", "in", "some", "way", "in", "a", "well", "dimensioned",
      "hash", "table", "the", "average", "cost", "number", "of",
      "instructions", "for", "each", "lookup", "is", "independent", "of",
      "the", "number", "of", "elements", "stored", "in", "the", "table",
      "many", "hash", "table", "designs", "also", "allow", "arbitrary",
      "insertions", "and", "deletions", "of", "key", "value", "pairs", "at",
      "amortized", "constant", "average", "cost", "per", "operation", "in",
      "many", "situations", "hash", "tables", "turn", "out", "to", "be",
      "more", "efficient", "than", "search", "trees", "or", "any", "other",
      "table", "lookup", "structure", "for", "this", "reason", "they", "are",
      "widely", "used", "in", "many", "kinds", "of", "computer", "software",
      "particularly", "for", "associative", "arrays", "database", "indexing",
      "caches", "and", "sets",

    "hashing",
      "the", "idea", "of", "hashing", "is", "to", "distribute", "the",
      "entries", "key", "value", "pairs", "across", "an", "array", "of",
      "buckets", "given", "a", "key", "the", "algorithm", "computes", "an",
      "index", "that", "suggests", "where", "the", "entry", "can", "be",
      "found", "index", "f", "key", "array", "size", "often", "this", "is",
      "done", "in", "two", "steps", "hash", "hashfunc", "key", "index", "hash",
      "array", "size", "in", "this", "method", "the", "hash", "is",
      "independent", "of", "the", "array", "size", "and", "it", "is", "then",
      "reduced", "to", "an", "index", "a", "number", "between", "and", "array",
      "size", "using", "the", "modulus", "operator", "in", "the", "case",
      "that", "the", "array", "size", "is", "a", "power", "of", "two", "the",
      "remainder", "operation", "is", "reduced", "to", "masking", "which",
      "improves", "speed", "but", "can", "increase", "problems", "with", "a",
      "poor", "hash", "function",

    "choosing", "a", "good", "hash", "function",
      "a", "good", "hash", "function", "and", "implementation", "algorithm",
      "are", "essential", "for", "good", "hash", "table", "performance", "but",
      "may", "be", "difficult", "to", "achieve", "a", "basic", "requirement",
      "is", "that", "the", "function", "should", "provide", "a", "uniform",
      "distribution", "of", "hash", "values", "a", "non", "uniform",
      "distribution", "increases", "the", "number", "of", "collisions", "and",
      "the", "cost", "of", "resolving", "them", "uniformity", "is",
      "sometimes", "difficult", "to", "ensure", "by", "design", "but", "may",
      "be", "evaluated", "empirically", "using", "statistical", "tests", "e",
      "g", "a", "pearson", "s", "chi", "squared", "test", "for", "discrete",
      "uniform", "distributions", "the", "distribution", "needs", "to", "be",
      "uniform", "only", "for", "table", "sizes", "that", "occur", "in", "the",
      "application", "in", "particular", "if", "one", "uses", "dynamic",
      "resizing", "with", "exact", "doubling", "and", "halving", "of", "the",
      "table", "size", "s", "then", "the", "hash", "function", "needs", "to",
      "be", "uniform", "only", "when", "s", "is", "a", "power", "of", "two",
      "on", "the", "other", "hand", "some", "hashing", "algorithms", "provide",
      "uniform", "hashes", "only", "when", "s", "is", "a", "prime", "number",
      "for", "open", "addressing", "schemes", "the", "hash", "function",
      "should", "also", "avoid", "clustering", "the", "mapping", "of", "two",
      "or", "more", "keys", "to", "consecutive", "slots", "such", "clustering",
      "may", "cause", "the", "lookup", "cost", "to", "skyrocket", "even", "if",
      "the", "load", "factor", "is", "low", "and", "collisions", "are",
      "infrequent", "the", "popular", "multiplicative", "hash", "3", "is",
      "claimed", "to", "have", "particularly", "poor", "clustering",
      "behavior", "cryptographic", "hash", "functions", "are", "believed",
      "to", "provide", "good", "hash", "functions", "for", "any", "table",
      "size", "s", "either", "by", "modulo", "reduction", "or", "by", "bit",
      "masking", "they", "may", "also", "be", "appropriate", "if", "there",
      "is", "a", "risk", "of", "malicious", "users", "trying", "to",
      "sabotage", "a", "network", "service", "by", "submitting", "requests",
      "designed", "to", "generate", "a", "large", "number", "of", "collisions",
      "in", "the", "server", "s", "hash", "tables", "however", "the", "risk",
      "of", "sabotage", "can", "also", "be", "avoided", "by", "cheaper",
      "methods", "such", "as", "applying", "a", "secret", "salt", "to", "the",
      "data", "or", "using", "a", "universal", "hash", "function",

    "perfect", "hash", "function",
      "if", "all", "keys", "are", "known", "ahead", "of", "time", "a",
      "perfect", "hash", "function", "can", "be", "used", "to", "create", "a",
      "perfect", "hash", "table", "that", "has", "no", "collisions", "if",
      "minimal", "perfect", "hashing", "is", "used", "every", "location", "in",
      "the", "hash", "table", "can", "be", "used", "as", "well", "perfect",
      "hashing", "allows", "for", "constant", "time", "lookups", "in", "the",
      "worst", "case", "this", "is", "in", "contrast", "to", "most",
      "chaining", "and", "open", "addressing", "methods", "where", "the",
      "time", "for", "lookup", "is", "low", "on", "average", "but", "may",
      "be", "very", "large", "proportional", "to", "the", "number", "of",
      "entries", "for", "some", "sets", "of", "keys"
  ]

  var Dict: Dictionary<Box<String>, Box<Bool>> = [:]
  let N = 5*scale

  // Check performance of filling the dictionary:
  for _ in 1...N {
    Dict = [:]
    for word in Input {
      Dict[Box(word)] = Box(true)
    }
  }
  CheckResults(Dict.count == 270)

  // Check performance of searching in the dictionary:
  // Fill the dictionary with words from the first half of the text
  Dict = [:]
  for i in 0 ..< Input.count/2 {
    let word = Input[i]
    Dict[Box(word)] = Box(true)
  }

  // Count number of words from the first half in the entire text
  var count = 0
  for _ in 1...N {
    for word in Input {
      if Dict[Box(word)] != nil {
        count += 1
      }
    }
  }
  CheckResults(count == N*541)
}
