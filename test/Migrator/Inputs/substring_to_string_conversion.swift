let s = "foo"
let ss = s[s.startIndex..<s.endIndex]

// CTP_ReturnStmt

func returnSubstringButWantedString() -> String {
  return ss
}

// CTP_DefaultParameter

// Never do this.
func defaultArgIsString(s: String = ss) {}

// CTP_CalleeResult
func returnSubstring() -> Substring {
  return ss
}
let s2: String = returnSubstring()

// CTP_CallArgument
func takeString(_ s: String) {}

takeString(s)
takeString(ss)

// CTP_ClosureResult
[1,2,3].map { (x: Int) -> String in
  return ss
}

// CTP_ArrayElement
let a: [String] = [ ss ]

// CTP_DictionaryKey
// CTP_DictionaryValue
let d: [String : String] = [
  "CTP_DictionaryValue" : ss,
  ss : "CTP_DictionaryKey",
]

// CTP_CoerceOperand
let s3: String = ss as String

// CTP_AssignSource
let s4: String = ss
