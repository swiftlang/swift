let s = "foo"
let ss = s[s.startIndex..<s.endIndex]

// CTP_ReturnStmt
func returnStringButWantedSubstring() -> Substring {
  return s
}

// CTP_DefaultParameter
// Never do this.
func defaultArgIsSubstring(ss: Substring = s) {}

// CTP_CalleeResult
func returnString() -> String {
  return s
}
let s2: Substring = returnString()

// CTP_CallArgument
func takeString(s: String) {}
func takeSubstring(ss: Substring) {}

takeSubstring(ss)
takeSubstring(s)

// CTP_ClosureResult
[1,2,3].map { (x: Int) -> Substring in
  return s
}

// CTP_ArrayElement
let a: [Substring] = [ s ]

// CTP_DictionaryKey
// CTP_DictionaryValue
let d: [Substring : Substring] = [
  "CTP_DictionaryValue" : s,
  s : "CTP_DictionaryKey",
]

// CTP_CoerceOperand
let s3: Substring = s as Substring

// CTP_AssignSource
let s4: Substring = s

