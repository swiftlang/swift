// RUN: not %target-swift-frontend -typecheck -primary-file %s -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK-LABEL: .hexEncodeBytes@
// CHECK-NEXT: <T where T : Collection, T.[Sequence]Element == UInt8>
func hexEncodeBytes<T: Collection>(_ bytes: T) where T.Generator.Element == UInt8 { }
