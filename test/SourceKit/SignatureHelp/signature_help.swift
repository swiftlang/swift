// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=18:10 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
struct Calculator {
  /// Adds two numbers together.
  ///
  /// - Parameters:
  ///   - x: The first number to add.
  ///   - y: The second number to add.
  /// - Returns: The sum of x and y.
  func add(_ x: Int, to y: Int) -> Int {
    return x + y
  }
  
  func add(first: Double, second: Float) -> Double {
    return first + Double(second)
  }
}

let calc = Calculator()
calc.add()

//--- expected.result
{
  key.signatures: [
    {
      key.name: "add(_ x: Int, to: Int) -> Int",
      key.doc_comment: "Adds two numbers together.\n\n- Parameters:\n  - x: The first number to add.\n  - y: The second number to add.\n- Returns: The sum of x and y.",
      key.parameters: [
        {
          key.name: "x",
          key.nameoffset: 4,
          key.namelength: 8
        },
        {
          key.name: "y",
          key.nameoffset: 14,
          key.namelength: 7
        }
      ],
      key.active_parameter: 0
    },
    {
      key.name: "add(first: Double, second: Float) -> Double",
      key.parameters: [
        {
          key.name: "first",
          key.nameoffset: 4,
          key.namelength: 13
        },
        {
          key.name: "second",
          key.nameoffset: 19,
          key.namelength: 13
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
