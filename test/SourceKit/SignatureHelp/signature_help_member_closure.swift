// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=5:23 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct Observable {
  var observer: (String, Int?, [AnyHashable: [Double?]]) async throws -> [Observable?]
  
  func notify() async throws {
    observer("EVENT", , [:])
  }
}

//--- expected.result
{
  key.signatures: [
    {
      key.name: "observer(String, Int?, [AnyHashable : [Double?]]) async throws -> [Observable?]",
      key.parameters: [
        {
          key.nameoffset: 9,
          key.namelength: 6
        },
        {
          key.nameoffset: 17,
          key.namelength: 4
        },
        {
          key.nameoffset: 23,
          key.namelength: 25
        }
      ],
      key.active_parameter: 1
    }
  ],
  key.active_signature: 0
}
