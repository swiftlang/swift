// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=8:15 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct Vector<Value> {
  init(elements: [Value]) { }
  
  func dot(with other: Vector<Value>) -> Value { }
}

let vec = Vector(elements: [1.0, 2.1, 3.4])
vec.dot(with: )

//--- expected.result
{
  key.signatures: [
    {
      key.name: "dot(with: Vector<Double>) -> Double",
      key.parameters: [
        {
          key.name: "other",
          key.nameoffset: 4,
          key.namelength: 20
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
