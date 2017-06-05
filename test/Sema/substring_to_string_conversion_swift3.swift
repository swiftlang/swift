// RUN: %target-swift-frontend -typecheck -verify -swift-version 3 %s

let s = "Hello"
let ss = s[s.startIndex..<s.endIndex]

// CTP_Initialization
do {
  let s1: String = { return ss }()
  _ = s1
}

// CTP_ReturnStmt
do {
  func returnsAString() -> String {
    return ss
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
  func foo(x: String = ss) {}
}

// CTP_CalleeResult
do {
  func getSubstring() -> Substring { return ss } // expected-error {{cannot convert return expression of type 'String' to return type 'Substring'}}
  let gottenString : String = getSubstring() // expected-error {{cannot convert value of type 'Substring' to specified type 'String'}} {{31-31=String(}} {{45-45=)}}
  _ = gottenString
}

// CTP_CallArgument
do {
  func takesAString(_ s: String) {}
  takesAString(ss)
}

// CTP_ClosureResult
do {
  [ss].map { (x: Substring) -> String in x } // expected-error {{declared closure result 'String' is incompatible with contextual type '_'}}
}

// CTP_ArrayElement
do {
  let a: [String] = [ ss ]
  _ = a
}

// CTP_DictionaryKey
do {
  let d: [ String : String ] = [ ss : s ]
  _ = d
}

// CTP_DictionaryValue
do {
  let d: [ String : String ] = [ s : ss ]
  _ = d
}

// CTP_CoerceOperand
do {
  let s1: String = ss as String
  _ = s1
}

// CTP_AssignSource
do {
  let s1: String = ss
  _ = s1
}

func takesString(_ s: String) {}

func apply(_ fn: (String) -> (), _ s: String) {
  fn(s[s.startIndex..<s.endIndex])
  let _: String = s[s.startIndex..<s.endIndex]
  _ = s[s.startIndex..<s.endIndex] as String
}
