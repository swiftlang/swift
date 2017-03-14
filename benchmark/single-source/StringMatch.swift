//===--- StringMatch.swift-------------------------------------------------===//
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

/* match: search for regexp anywhere in text */
func match(regexp: String, text: String) -> Bool {
  if regexp.first == "^" {
    return matchHere(regexp.dropFirst(), text)
  }
  
  var idx = text.startIndex
  while true {  // must look even if string is empty
    if matchHere(regexp, text[idx..<text.endIndex]) {
      return true
    }
    guard idx != text.endIndex else { break }
    // do while sufficed in the original C version...
    text.formIndex(after: &idx)
  } // while idx++ != string.endIndex
  
  return false
}

/* matchhere: search for regexp at beginning of text */
func matchHere(_ regexp: String, _ text: String) -> Bool {
  if regexp.isEmpty {
    return true
  }
  
  if let c = regexp.first, regexp.dropFirst().first == "*" {
    return matchStar(c, regexp.dropFirst(2), text)
  }
  
  if regexp.first == "$" && regexp.dropFirst().isEmpty {
    return text.isEmpty
  }
  
  if let tc = text.first, let rc = regexp.first, rc == "." || tc == rc {
    return matchHere(regexp.dropFirst(), text.dropFirst())
  }
  
  return false
}

/* matchstar: search for c*regexp at beginning of text */
func matchStar(_ c: Character, _ regexp: String, _ text: String) -> Bool {
  var idx = text.startIndex
  while true {   /* a * matches zero or more instances */
    if matchHere(regexp, text[idx..<text.endIndex]) {
      return true
    }
    if idx == text.endIndex || (text[idx] != c && c != ".") {
      return false
    }
    text.formIndex(after: &idx)
  }
}

let tests: DictionaryLiteral = [
  "^h..lo*!$":"hellooooo!",
  "^h..lo*!$":"hella noms",
  ".ab":"abracadabra!",
  "s.*":"saaaad!",
  "...e.$":"\"Ganymede,\" he continued, \"is the largest moon in the Solar System\"",
  "🤠*":"even 🤠🤠🤠 are supported",
]

@inline(never)
public func run_StringMatch(_ N: Int) {
  for _ in 1...N*100 {
    for (regex, text) in tests {
      _ = match(regexp: regex,text: text)
    }
  }
}

