// RUN: %target-swift-frontend -typecheck -verify -fix-string-substring-conversion -swift-version 4 %s

let s = "Hello"
let ss = s[s.startIndex..<s.endIndex]

// CTP_Initialization
do {
  let s1: Substring = { return s }() // expected-error {{cannot convert value of type 'String' to closure result type 'Substring'}} {{33-33=[]}}
  _ = s1
}

// CTP_ReturnStmt
do {
  func returnsASubstring() -> Substring {
    return s // expected-error {{cannot convert return expression of type 'String' to return type 'Substring'}} {{13-13=[]}}
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
  func foo(x: Substring = s) {} // expected-error {{default argument value of type 'String' cannot be converted to type 'Substring'}} {{28-28=[]}}
}

// CTP_CalleeResult
do {
  func getString() -> String { return s }
  let gottenSubstring: Substring = getString() // expected-error {{cannot convert value of type 'String' to specified type 'Substring'}} {{47-47=[]}}
  _ = gottenSubstring
}

// CTP_CallArgument
do {
  func takesASubstring(_ ss: Substring) {}
  takesASubstring(s) // expected-error {{cannot convert value of type 'String' to expected argument type 'Substring'}} {{20-20=[]}}
}

// CTP_ClosureResult
do {
  [s].map { (x: String) -> Substring in x } // expected-error {{cannot convert value of type 'String' to closure result type 'Substring'}} {{42-42=[]}}
}

// CTP_ArrayElement
do {
  let a: [Substring] = [ s ] // expected-error {{cannot convert value of type 'String' to expected element type 'Substring'}} {{27-27=[]}}
  _ = a
}

// CTP_DictionaryKey
do {
  let d: [ Substring : Substring ] = [ s : ss ] // expected-error {{cannot convert value of type 'String' to expected dictionary key type 'Substring'}} {{41-41=[]}}
  _ = d
}

// CTP_DictionaryValue
do {
  let d: [ Substring : Substring ] = [ ss : s ] // expected-error {{cannot convert value of type 'String' to expected dictionary value type 'Substring'}} {{46-46=[]}}
  _ = d
}

// CTP_CoerceOperand
do {
  let s1: Substring = s as Substring // expected-error {{cannot convert value of type 'String' to type 'Substring' in coercion}} {{24-24=[]}}
  _ = s1
}

// CTP_AssignSource
do {
  let s1: Substring = s // expected-error {{cannot convert value of type 'String' to specified type 'Substring'}} {{24-24=[]}}
  _ = s1
}

