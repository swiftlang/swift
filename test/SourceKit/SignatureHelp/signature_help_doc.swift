// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=17:8 %t/input.swift -- %t/input.swift > %t/actual.result
// RUN: diff -u %t/expected.result %t/actual.result

//--- input.swift
/// Adds two integers.
///
/// - Parameters:
///   - x: The first integer to add.
///   - y: The second integer to add.
///
/// Usage:
/// ```swift
/// add(1, to: 2) // 3
/// ```
///
/// - Returns: The sum of the two integers.
func add(_ x: Int, to y: Int) -> Int {
  return x + y
}

add(x: )

//--- expected.result
{
  key.signatures: [
    {
      key.name: "add(_ x: Int, to: Int) -> Int",
      key.doc_comment: "Adds two integers.\n\n- Parameters:\n  - x: The first integer to add.\n  - y: The second integer to add.\n\nUsage:\n```swift\nadd(1, to: 2) // 3\n```\n\n- Returns: The sum of the two integers.",
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
    }
  ],
  key.active_signature: 0
}
