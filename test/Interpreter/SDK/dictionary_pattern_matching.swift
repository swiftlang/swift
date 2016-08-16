// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

// REQUIRES: objc_interop

import Foundation

struct State {
  let name: String
  let population: Int
  let abbrev: String
}

func stateFromPlistLame(_ plist: Dictionary<String, Any>) -> State? {
  if let name = plist["name"] as? NSString {
    if let population = plist["population"] as? NSNumber {
      if let abbrev = plist["abbrev"] as? NSString {
        if abbrev.length == 2 {
          return State(name: name as String,
                       population: population.intValue,
                       abbrev: abbrev as String)
        }
      }
    }
  }
  return nil
}

func stateFromPlistCool(_ plist: Dictionary<String, Any>) -> State? {
  switch (plist["name"], plist["population"], plist["abbrev"]) {
  case let (name as String, pop as Int, abbr as String)
  where abbr.characters.count == 2:
    return State(name: name,
                 population: pop,
                 abbrev: abbr)
  default:
    return nil
  }
}

let goodStatePlist: Dictionary<String, Any> = [
  "name" as String: "California",
  "population" as String: 38_040_000,
  "abbrev" as String: "CA",
]
let invalidStatePlist1: Dictionary<String, Any> = [
  "name": "California",
  "population": "hella",
  "abbrev": "CA",
]
let invalidStatePlist2: Dictionary<String, Any> = [
  "name": "California",
  "population": 38_040_000,
  "abbrev": "Cali",
]
let invalidStatePlist3: Dictionary<String, Any> = [
  "name": "California",
  "population": 38_040_000,
]

// CHECK-LABEL: some:
// CHECK:         name: "California"
// CHECK:         population: 38040000
// CHECK:         abbrev: "CA"
dump(stateFromPlistLame(goodStatePlist))
// CHECK-LABEL: some:
// CHECK:         name: "California"
// CHECK:         population: 38040000
// CHECK:         abbrev: "CA"
dump(stateFromPlistCool(goodStatePlist))
// CHECK-LABEL: nil
dump(stateFromPlistLame(invalidStatePlist1))
// CHECK-LABEL: nil
dump(stateFromPlistCool(invalidStatePlist1))
// CHECK-LABEL: nil
dump(stateFromPlistLame(invalidStatePlist2))
// CHECK-LABEL: nil
dump(stateFromPlistCool(invalidStatePlist2))
// CHECK-LABEL: nil
dump(stateFromPlistLame(invalidStatePlist3))
// CHECK-LABEL: nil
dump(stateFromPlistCool(invalidStatePlist3))

struct Country {
  let name: String
  let population: Int
}

enum Statistic : CustomReflectable {
  case ForState(State)
  case ForCountry(Country)

  var customMirror: Mirror {
    switch self {
      case .ForState(let state):
        return Mirror(
          self, children: ["State": state], displayStyle: .`enum`)
      case .ForCountry(let country):
        return Mirror(
          self, children: ["Country": country], displayStyle: .`enum`)
    }
  }
}

func statisticFromPlist(_ plist: Dictionary<String, Any>) -> Statistic? {
  switch (plist["kind"], plist["name"], plist["population"], plist["abbrev"]) {
  case let ("state" as String, name as String, population as Int, abbrev as String)
  where abbrev.characters.count == 2:
    return Statistic.ForState(State(name: name,
                                    population: population,
                                    abbrev: abbrev))
  case let ("country" as String, name as String, population as Int, .none):
    return Statistic.ForCountry(Country(name: name,
                                        population: population))
  default:
    return nil
  }
}

let goodStatePlist2: Dictionary<String, Any> = [
  "kind": "state",
  "name": "California",
  "population": 38_040_000,
  "abbrev": "CA"
]
let goodCountryPlist: Dictionary<String, Any> = [
  "kind": "country",
  "name": "India",
  "population": 1_23_70_00_000,
]
let invalidCountryPlist1: Dictionary<String, Any> = [
  "kind": "country",
  "name": "India",
  "population": 1_23_70_00_000,
  "abbrev": "IN"
]
let invalidCountryPlist2: Dictionary<String, Any> = [
  "kind": "country",
  "name": "India",
  "population": "123 crore",
]
let invalidKindPlist: Dictionary<String, Any> = [
  "kind": "planet",
  "name": "Mercury",
  "population": 0
]

// CHECK-LABEL: ▿ Optional(main.Statistic.ForState(main.State(name: "California", population: 38040000, abbrev: "CA")))
dump(statisticFromPlist(goodStatePlist2))
// CHECK-LABEL: ▿ Optional(main.Statistic.ForCountry(main.Country(name: "India", population: 1237000000)))
dump(statisticFromPlist(goodCountryPlist))
// CHECK-LABEL: nil
dump(statisticFromPlist(invalidCountryPlist1))
// CHECK-LABEL: nil
dump(statisticFromPlist(invalidCountryPlist2))
// CHECK-LABEL: nil
dump(statisticFromPlist(invalidKindPlist))
