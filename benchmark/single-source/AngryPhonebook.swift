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
  "Bobby", "Dylan", "Johnny", "Phillip", "Craig"]

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
