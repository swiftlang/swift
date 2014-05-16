// RUN: %target-run-simple-swift | FileCheck %s

import Foundation

struct State {
  let name: String
  let population: Int
  let abbrev: String
}

func stateFromPlistLame(plist: Dictionary<String, AnyObject>) -> State? {
  if let name = plist["name"] as NSString {
    if let population = plist["population"] as NSNumber {
      if let abbrev = plist["abbrev"] as NSString {
        if abbrev.length == 2 {
          return State(name: name,
                       population: population.integerValue,
                       abbrev: abbrev)
        }
      }
    }
  }
  return nil
}

func stateFromPlistCool(plist: Dictionary<String, AnyObject>) -> State? {
  switch (plist["name"], plist["population"], plist["abbrev"]) {
  case let (.Some(name as NSString), .Some(pop as NSNumber), .Some(abbr as NSString))
  where abbr.length == 2:
    return State(name: name,
                 population: pop.integerValue,
                 abbrev: abbr)
  default:
    return nil
  }
}

let goodStatePlist: Dictionary<String, AnyObject> = [
  "name": "California",
  "population": 38_040_000,
  "abbrev": "CA",
]
let invalidStatePlist1: Dictionary<String, AnyObject> = [
  "name": "California",
  "population": "hella",
  "abbrev": "CA",
]
let invalidStatePlist2: Dictionary<String, AnyObject> = [
  "name": "California",
  "population": 38_040_000,
  "abbrev": "Cali",
]
let invalidStatePlist3: Dictionary<String, AnyObject> = [
  "name": "California",
  "population": 38_040_000,
]

// CHECK-LABEL: Some:
// CHECK:         name: California
// CHECK:         population: 38040000
// CHECK:         abbrev: CA
dump(stateFromPlistLame(goodStatePlist))
// CHECK-LABEL: Some:
// CHECK:         name: California
// CHECK:         population: 38040000
// CHECK:         abbrev: CA
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

enum Statistic: Reflectable {
  case ForState(State)
  case ForCountry(Country)

  func getMirror() -> Mirror {
    return StatMirror(_value: self)
  }
}

struct StatMirror: Mirror {
  let _value: Statistic

  var value: Any { return _value }
  var valueType: Any.Type { return value.dynamicType }
  var objectIdentifier: ObjectIdentifier? { return nil }
  var count: Int { return 1 }

  subscript(i: Int) -> (String, Mirror) {
    assert(i == 0)
    switch _value {
    case .ForState(let state):
      return ("State", reflect(state))
    case .ForCountry(let country):
      return ("Country", reflect(country))
    }
  }

  var summary: String {
    switch _value {
    case .ForState:
      return "State"
    case .ForCountry:
      return "Country"
    }
  }

  var quickLookObject: QuickLookObject? { return nil }
  var disposition: MirrorDisposition { return .Enum }
}

func statisticFromPlist(plist: Dictionary<String, AnyObject>) -> Statistic? {
  switch (plist["kind"], plist["name"], plist["population"], plist["abbrev"]) {
  case let (.Some("state" as NSString),
            .Some(name as NSString),
            .Some(population as NSNumber),
            .Some(abbrev as NSString))
  where abbrev.length == 2:
    return Statistic.ForState(State(name: name,
                                    population: population.integerValue,
                                    abbrev: abbrev))
  case let (.Some("country" as NSString),
            .Some(name as NSString),
            .Some(population as NSNumber),
            .None):
    return Statistic.ForCountry(Country(name: name,
                                        population: population.integerValue))
  default:
    return nil
  }
}

let goodStatePlist2: Dictionary<String, AnyObject> = [
  "kind": "state",
  "name": "California",
  "population": 38_040_000,
  "abbrev": "CA"
]
let goodCountryPlist: Dictionary<String, AnyObject> = [
  "kind": "country",
  "name": "India",
  "population": 1_23_70_00_000,
]
let invalidCountryPlist1: Dictionary<String, AnyObject> = [
  "kind": "country",
  "name": "India",
  "population": 1_23_70_00_000,
  "abbrev": "IN"
]
let invalidCountryPlist2: Dictionary<String, AnyObject> = [
  "kind": "country",
  "name": "India",
  "population": "123 crore",
]
let invalidKindPlist: Dictionary<String, AnyObject> = [
  "kind": "planet",
  "name": "Mercury",
  "population": 0
]

// CHECK-LABEL: Some: State
dump(statisticFromPlist(goodStatePlist2))
// CHECK-LABEL: Some: Country
dump(statisticFromPlist(goodCountryPlist))
// CHECK-LABEL: nil
dump(statisticFromPlist(invalidCountryPlist1))
// CHECK-LABEL: nil
dump(statisticFromPlist(invalidCountryPlist2))
// CHECK-LABEL: nil
dump(statisticFromPlist(invalidKindPlist))
