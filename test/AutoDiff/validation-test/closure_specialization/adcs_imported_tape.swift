// REQUIRES: executable_test
// RUN: %empty-directory(%t)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ADCSLibrary)) %s -D LIBRARY -O -emit-module -emit-module-path %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-build-swift %s %S/Inputs/adcs_imported_tape_other.swift -parse-as-library -I %t -L %t -lADCSLibrary -O -Xfrontend -sil-verify-all -o %t/client %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(ADCSLibrary)
// RUN: %target-codesign %t/client
// RUN: %target-run %t/client %t/%target-library-name(ADCSLibrary)
// RUN: %target-build-swift-dylib(%t/%target-library-name(ADCSLibrary)) %s -D LIBRARY -O -enable-library-evolution -emit-module -emit-module-path %t/ADCSLibrary.swiftmodule -module-name ADCSLibrary
// RUN: %target-build-swift %s %S/Inputs/adcs_imported_tape_other.swift -parse-as-library -I %t -L %t -lADCSLibrary -O -enable-library-evolution -Xfrontend -sil-verify-all -o %t/resilient-client %target-rpath(%t)
// RUN: %target-codesign %t/%target-library-name(ADCSLibrary)
// RUN: %target-codesign %t/resilient-client
// RUN: %target-run %t/resilient-client %t/%target-library-name(ADCSLibrary)

import _Differentiation

#if LIBRARY

// Specializing the imported generic VJP synthesizes new tape declarations in
// the client. They must have a fixed layout, including the empty predecessors.
@inlinable
@differentiable(reverse, wrt: x)
public func importedBranch<T>(_ x: Float, _ marker: T, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}

#else

import ADCSLibrary

@differentiable(reverse, wrt: x)
public func clientInt(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Int(1), flag)
}

@differentiable(reverse, wrt: x)
public func clientDouble(_ x: Float, _ flag: Bool) -> Float {
  importedBranch(x, Double(1), flag)
}

#if !DEFINITIONS_ONLY
// clientOther is compiled in another primary file. Its specialization of the
// same imported VJP must link alongside this file's synthesized tape metadata.
@main
struct ImportedTapeTests {
  static func main() {
    for flag in [true, false] {
      let integer = valueWithPullback(at: Float(3), of: { clientInt($0, flag) })
      let floating = valueWithPullback(at: Float(3), of: { clientDouble($0, flag) })
      let other = valueWithPullback(at: Float(3), of: { clientOther($0, flag) })
      precondition(integer.value == (flag ? 9 : 6))
      precondition(floating.value == (flag ? 9 : 6))
      precondition(other.value == (flag ? 9 : 6))
      for seed in [Float(1), 5, 2] {
        let expected = (flag ? 6 : 2) * seed
        precondition(integer.pullback(seed) == expected)
        precondition(floating.pullback(seed) == expected)
        precondition(other.pullback(seed) == expected)
      }
    }
  }
}
#endif
#endif
