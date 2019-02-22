// RUN: %target-swift-frontend -typecheck -verify -swift-version 4 %s

let s = "Hello"
let ss = s[s.startIndex..<s.endIndex]

// CTP_Initialization
do {
  let s1: String = { return ss }() // expected-error {{cannot convert value of type 'Substring' to closure result type 'String'}} {{29-29=String(}} {{31-31=)}}
  _ = s1
}

// CTP_ReturnStmt
do {
  func returnsAString() -> String {
    return ss // expected-error {{cannot convert return expression of type 'Substring' to return type 'String'}} {{12-12=String(}} {{14-14=)}}
  }
}

// CTP_ThrowStmt
// Doesn't really make sense for this fix-it - see case in diagnoseContextualConversionError:
// The conversion destination of throw is always ErrorType (at the moment)
// if this ever expands, this should be a specific form like () is for
// return.

// CTP_EnumCaseRawValue
// Substrings can't be raw values because they aren't literals.

// CTP_DefaultParameter
do {
  func foo(x: String = ss) {} // expected-error {{default argument value of type 'Substring' cannot be converted to type 'String'}} {{24-24=String(}} {{26-26=)}}
}

// CTP_CalleeResult
do {
  func getSubstring() -> Substring { return ss }
  let gottenString : String = getSubstring() // expected-error {{cannot convert value of type 'Substring' to specified type 'String'}} {{31-31=String(}} {{45-45=)}}
  _ = gottenString
}

// CTP_CallArgument
do {
  func takesAString(_ s: String) {}
  takesAString(ss) // expected-error {{cannot convert value of type 'Substring' to expected argument type 'String'}} {{16-16=String(}} {{18-18=)}}
}

// CTP_ClosureResult
do {
  [ss].map { (x: Substring) -> String in x } // expected-error {{cannot convert value of type 'Substring' to closure result type 'String'}} {{42-42=String(}} {{43-43=)}}
}

// CTP_ArrayElement
do {
  let a: [String] = [ ss ] // expected-error {{cannot convert value of type 'Substring' to expected element type 'String'}} {{23-23=String(}} {{25-25=)}}
  _ = a
}

// CTP_DictionaryKey
do {
  let d: [ String : String ] = [ ss : s ] // expected-error {{cannot convert value of type 'Substring' to expected dictionary key type 'String'}} {{34-34=String(}} {{36-36=)}}
  _ = d
}

// CTP_DictionaryValue
do {
  let d: [ String : String ] = [ s : ss ] // expected-error {{cannot convert value of type 'Substring' to expected dictionary value type 'String'}} {{38-38=String(}} {{40-40=)}}
  _ = d
}

// CTP_CoerceOperand
do {
  let s1: String = ss as String // expected-error {{cannot convert value of type 'Substring' to type 'String' in coercion}} {{20-20=String(}} {{22-22=)}}
  _ = s1
}

// CTP_AssignSource
do {
  let s1: String = ss // expected-error {{cannot convert value of type 'Substring' to specified type 'String'}} {{20-20=String(}} {{22-22=)}}
  _ = s1
}

// Substring-to-String via subscripting in a context expecting String
func takesString(_ s: String) {}

func apply(_ fn: (String) -> (), _ s: String) {
  fn(s[s.startIndex..<s.endIndex]) // expected-error{{subscripts returning String were obsoleted in Swift 4; explicitly construct a String from subscripted result}} {{6-6=String(}} {{34-34=)}}
  let _: String = s[s.startIndex..<s.endIndex] // expected-error{{subscripts returning String were obsoleted in Swift 4; explicitly construct a String from subscripted result}} {{19-19=String(}} {{47-47=)}}
  _ = s[s.startIndex..<s.endIndex] as String // expected-error{{subscripts returning String were obsoleted in Swift 4; explicitly construct a String from subscripted result}} {{7-7=String(}} {{35-35=)}}
}

// rdar://33474838
protocol Derivable {
  func derive() -> Substring
}
func foo<T: Derivable>(t: T) -> String {
  return t.derive()  // expected-error {{cannot convert return expression of type 'Substring' to return type 'String'}} {{10-10=String(}} {{20-20=)}}
}
