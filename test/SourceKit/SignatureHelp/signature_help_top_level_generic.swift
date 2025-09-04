// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=5:27 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
func add<T>(x: T, y: T, with adder: (T, T) -> T) -> T where T: AdditiveArithmetic {
  return adder(x, y)
}

add(x: "A", y: "B", with: )

//--- expected.result
{
  key.signatures: [
    {
      key.name: "add(x: String, y: String, with: (String, String) -> String) -> String",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 9
        },
        {
          key.nameoffset: 15,
          key.namelength: 9
        },
        {
          key.nameoffset: 26,
          key.namelength: 32
        }
      ],
      key.active_parameter: 2
    }
  ],
  key.active_signature: 0
}
