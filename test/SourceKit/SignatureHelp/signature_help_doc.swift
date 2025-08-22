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

// RUN: %sourcekitd-test -req=signaturehelp -pos=17:8 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.signatures: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(_ x: Int, to: Int) -> Int",
// CHECK-NEXT:       key.doc_comment: "Adds two integers.\n\n- Parameters:\n  - x: The first integer to add.\n  - y: The second integer to add.\n\nUsage:\n```swift\nadd(1, to: 2) // 3\n```\n\n- Returns: The sum of the two integers.",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 14,
// CHECK-NEXT:           key.namelength: 7
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.active_signature: 0
// CHECK-NEXT: }
