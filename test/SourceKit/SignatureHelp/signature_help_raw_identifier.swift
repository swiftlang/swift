// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=5:37 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct `Raw Identifier` {
  func `some method :)`(`param label?` `argument label!`: Int) {}
}

`Raw Identifier`().`some method :)`()

//--- expected.result
{
  key.signatures: [
    {
      key.name: "`some method :)`(`param label?`: Int)",
      key.parameters: [
        {
          key.nameoffset: 17,
          key.namelength: 19
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
