// RUN: %target-typecheck-verify-swift

class CustomDict: ExpressibleByDictionaryLiteral {
  typealias Key = String
  typealias Value = String
  
  required init(dictionaryLiteral elements: (String, String)...) {}
}
func fDict(_ d: [String: String]) {}
func fCustomDict(_ d: CustomDict) {}
func fDictGeneric<D>(_ d: D) where D: ExpressibleByDictionaryLiteral {}

// Duplicated 
let _ = [
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'A'}}
  "A": "B" // expected-note{{duplicate key declared here}} {{3-12=}} {{16:11-12=}}
]

let _: [String: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'A'}}
  "A": "B" // expected-note{{duplicate key declared here}} {{3-12=}} {{22:11-12=}}
]

let _: [String: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-15=}}{{15-16=}}
  (("A")): "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'A'}}
  "A": "B" // expected-note{{duplicate key declared here}} {{3-12=}} {{28:15-16=}}
]

let _: [String: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'A'}}
  "B": "B",
  "C": "C", 
  "A": "D" // expected-note{{duplicate key declared here}} {{3-12=}} {{36:11-12=}}
]

// Number literal key
let _: [Int: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-9=}} {{9-10=}}
  1: "1", // expected-warning{{dictionary literal of type '[Int : String]' has duplicate entries for integer literal key '1'}}
  2: "2",
  1: "3" // expected-note{{duplicate key declared here}} {{3-10=}} {{44:9-10=}}
]

let _: [Double: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-12=}} {{12-13=}}
  1.01: "1", // expected-warning{{dictionary literal of type '[Double : String]' has duplicate entries for floating-point literal key '1.01'}}
  2: "2",
  1.01: "3" // expected-note{{duplicate key declared here}} {{3-13=}} {{51:9-10=}}
]

// Boolean literal
let _: [Bool: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-12=}} {{12-13=}}
  true: "1", // expected-warning{{dictionary literal of type '[Bool : String]' has duplicate entries for boolean literal key 'true'}}
  false: "2",
  true: "3" // expected-note{{duplicate key declared here}} {{3-13=}} {{59:13-14=}}
]

// nil literal 
let _: [Int?: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  nil: "1", // expected-warning{{dictionary literal of type '[Int? : String]' has duplicate entries for nil literal key}}
  2: "2",
  nil: "3" // expected-note{{duplicate key declared here}} {{3-12=}} {{67:9-10=}}
]

// Nested
let _: [String: [String: String]] = [
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": [:], // expected-warning{{dictionary literal of type '[String : [String : String]]' has duplicate entries for string literal key 'A'}}
  "B": [:],
  "C": [:], 
  "A": [:] // expected-note{{duplicate key declared here}} {{3-12=}} {{76:11-12=}}
]

let _: [String: [String: String]] = [
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": [:], // expected-warning{{dictionary literal of type '[String : [String : String]]' has duplicate entries for string literal key 'A'}}
  "B": [:],
  "C": [:], 
  "A": ["a": "", "a": ""] // expected-note{{duplicate key declared here}} {{3-27=}} {{84:11-12=}}
  // expected-warning@-1{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'a'}}
  // expected-note@-2{{duplicate key declared here}} {{9-16=}} {{16-17=}}
  // expected-note@-3{{duplicate key declared here}} {{18-25=}} {{16-17=}}
]

// Parent OK, nested duplicated
let _: [String: [String: String]] = [
  "A": [:], 
  "B": [:],
  "C": [:], 
  // expected-note@+1{{duplicate key declared here}} {{9-16=}} {{16-17=}}
  "D": ["a": "", "a": ""] // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'a'}}
  // expected-note@-1{{duplicate key declared here}}{{18-25=}} {{18-25=}} {{16-17=}}
]

// Ok, because custom implementations of ExpressibleByDictionaryLiteral can allow duplicated keys.
let _: CustomDict = [
  "A": "A", 
  "A": "B"
]

fDict([
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'A'}}
  "A": "B" // expected-note{{duplicate key declared here}} {{3-12=}} {{109:11-12=}}
])
fCustomDict([
  "A": "A", 
  "A": "B"
])
fDictGeneric([
  // expected-note@+1{{duplicate key declared here}} {{3-11=}} {{11-12=}}
  "A": "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for string literal key 'A'}}
  "A": "B" // expected-note{{duplicate key declared here}} {{3-12=}} {{118:11-12=}}
])

// Magic literals
let _: [String: String] = [
  // expected-note@+1{{duplicate key declared here}} {{3-13=}} {{13-14=}}
  #file: "A", // expected-warning{{dictionary literal of type '[String : String]' has duplicate entries for #file literal key}}
  #file: "B" // expected-note{{duplicate key declared here}} {{3-14=}} {{125:13-14=}}
]


// OK
let _ = [
  "A": "A", 
  "B": "B"
]
let _: [String: String] = [
  "A": "A", 
  "B": "B"
]
let _: CustomDict = [
  "A": "A", 
  "B": "B"
]

// Number literal key
let _: [Int: String] = [
  1: "1",
  2: "2",
  3: "3"
]

let _: [Double: String] = [
  1.01: "1",
  2: "2",
  1.02: "3"
]

// Boolean literal
let _: [Bool: String] = [
  true: "1",
  false: "2"
]

// nil literal 
let _: [Int?: String] = [
  nil: "1",
  2: "2"
]

// Key as the same variable is OK, we only diagnose literals
let a = "A"

let _: [String: String] = [
  a: "A",
  "B": "B",
  "a": "C", 
  a: "D"
]

let _: [String: [String: String]] = [
  "A": [:],
  "B": [:],
  "C": [:], 
  "D": ["a": "", "b": ""]
]

fDict([
  "A": "A", 
  "B": "B"
])
fCustomDict([
  "A": "A", 
  "B": "B"
])
fDictGeneric([
  "A": "A", 
  "B": "B"
])

func magicFn() {
  let _: [String: String] = [
    #file: "A", 
    #function: "B"
  ]
}

// Interpolated literals
let _: [String: String] = [
  "\(a)": "A", 
  "\(a)": "B",
  "\(1)": "C"
]
