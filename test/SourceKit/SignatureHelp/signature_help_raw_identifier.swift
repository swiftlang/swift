// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=5:37 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct `Raw Identifier` {
  func `some method :)`(`argument label!` `param label?`: Int) {}
}

`Raw Identifier`().`some method :)`()

//--- expected.result
{
  key.signatures: [
    {
      key.name: "`some method :)`(`argument label!`: Int)",
      key.parameters: [
        {
          key.name: "param label?",
          key.nameoffset: 17,
          key.namelength: 22
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
