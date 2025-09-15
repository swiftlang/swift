// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=5:27 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct Person {
  init(name: String, age: Int, profession job: String) { }
}

Person(name: "John", age: )

//--- expected.result
{
  key.signatures: [
    {
      key.name: "init(name: String, age: Int, profession: String)",
      key.parameters: [
        {
          key.name: "name",
          key.nameoffset: 5,
          key.namelength: 12
        },
        {
          key.name: "age",
          key.nameoffset: 19,
          key.namelength: 8
        },
        {
          key.name: "job",
          key.nameoffset: 29,
          key.namelength: 18
        }
      ],
      key.active_parameter: 1
    }
  ],
  key.active_signature: 0
}
