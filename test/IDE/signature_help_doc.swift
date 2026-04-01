// RUN: %target-swift-ide-test -signature-help -code-completion-token=DOC -source-filename=%s | %FileCheck %s --check-prefix=DOC

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

add(#^DOC^#)
// DOC:     Begin signatures, 1 items
// DOC-DAG: Signature[Active]: add(<param name="x" active>_ x: Int</param>, <param name="y">to: Int</param>) -> Int; Documentation=Adds two integers.\n\n- Parameters:\n  - x: The first integer to add.\n  - y: The second integer to add.\n\nUsage:\n```swift\nadd(1, to: 2) // 3\n```\n\n- Returns: The sum of the two integers.
