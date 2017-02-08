//===--- Phonebook.swift --------------------------------------------------===//
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

// This test is based on util/benchmarks/Phonebook, with modifications
// for performance measuring.
import TestsUtils

var words = [
  "James", "John", "Robert", "Michael", "William", "David", "Richard", "Joseph",
  "Charles", "Thomas", "Christopher", "Daniel", "Matthew", "Donald", "Anthony",
  "Paul", "Mark", "George", "Steven", "Kenneth", "Andrew", "Edward", "Brian",
  "Joshua", "Kevin", "Ronald", "Timothy", "Jason", "Jeffrey", "Gary", "Ryan",
  "Nicholas", "Eric", "Stephen", "Jacob", "Larry", "Frank", "Jonathan", "Scott",
  "Justin", "Raymond", "Brandon", "Gregory", "Samuel", "Patrick", "Benjamin",
  "Jack", "Dennis", "Jerry", "Alexander", "Tyler", "Douglas", "Henry", "Peter",
  "Walter", "Aaron", "Jose", "Adam", "Harold", "Zachary", "Nathan", "Carl",
  "Kyle", "Arthur", "Gerald", "Lawrence", "Roger", "Albert", "Keith", "Jeremy",
  "Terry", "Joe", "Sean", "Willie", "Jesse", "Ralph", "Billy", "Austin", "Bruce",
  "Christian", "Roy", "Bryan", "Eugene", "Louis", "Harry", "Wayne", "Ethan",
  "Jordan", "Russell", "Alan", "Philip", "Randy", "Juan", "Howard", "Vincent",
  "Bobby", "Dylan", "Johnny", "Phillip", "Craig"
]

// This is a phone book record.
struct Record : Comparable {
  var first: String
  var last: String

  init(_ first_ : String,_ last_ : String) {
    first = first_
    last = last_
  }
}
func ==(lhs: Record, rhs: Record) -> Bool {
  return lhs.last == rhs.last && lhs.first == rhs.first
}

func <(lhs: Record, rhs: Record) -> Bool {
  if lhs.last < rhs.last {
    return true
  }
  if lhs.last > rhs.last {
    return false
  }

  if lhs.first < rhs.first {
    return true
  }

  return false
}

@inline(never)
public func run_Phonebook(_ N: Int) {
  // The list of names in the phonebook.
  var Names : [Record] = []
  for first in words {
    for last in words {
      Names.append(Record(first, last))
    }
  }
  for _ in 1...N {
    var t = Names
    t.sort()
  }
}
