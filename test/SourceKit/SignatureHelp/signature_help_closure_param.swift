// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=2:15 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
func apply<Value, Result>(value: Value, body: (Value) -> Result) -> Result {
  return body()
}

//--- expected.result
{
  key.signatures: [
    {
      key.name: "body(Value) -> Result",
      key.parameters: [
        {
          key.nameoffset: 5,
          key.namelength: 5
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
