// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library %s %S/Inputs/protocol-conformance-member-helper.swift -o %t/%target-library-name(Test) -module-name Test
// RUN: llvm-readobj -symbols -coff-exports %t/%target-library-name(Test) | %FileCheck %s

// CHECK: Name: {{_?}}$s4Test10CoolStructV10coolFactorSdvg

// SR-156: Make sure we synthesize getters for members used as protocol
// witnesses. Check that we link correctly; we don't care which file
// synthesizes it.

protocol CoolStructProtocol {
  var coolFactor: Double { get }
}
extension CoolStruct : CoolStructProtocol {}
