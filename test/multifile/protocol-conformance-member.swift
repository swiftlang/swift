// RUN: %empty-directory(%t)
// RUN: %target-build-swift -emit-library %s %S/Inputs/protocol-conformance-member-helper.swift -o %t/libTest.dylib -module-name Test
// RUN: llvm-nm %t/libTest.dylib | %FileCheck %s

// CHECK: $s4Test10CoolStructV10coolFactorSdvg

// SR-156: Make sure we synthesize getters for members used as protocol
// witnesses. Check that we link correctly; we don't care which file
// synthesizes it.

protocol CoolStructProtocol {
  var coolFactor: Double { get }
}
extension CoolStruct : CoolStructProtocol {}
