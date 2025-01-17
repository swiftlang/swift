// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all -enable-experimental-feature BuiltinModule -Xfrontend -disable-availability-checking) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_BuiltinModule

// Execute an unaligned load of SIMD16<UInt8> which retroactively conforms directly to BitwiseCopyable.

extension SIMD16 : @retroactive BitwiseCopyable where Scalar.SIMD16Storage : BitwiseCopyable {}

func doit() {
  let bytes: [UInt8] = Array(repeating: 0, count: 64)
  bytes.withUnsafeBufferPointer { bytes in
      let rawBytes = UnsafeRawPointer(bytes.baseAddress!) + 1
      let vector = rawBytes.loadUnaligned(as: SIMD16<UInt8>.self)
      //CHECK: SIMD16<UInt8>(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
      blackhole(vector)
  }
}

import Builtin

doit()

@_silgen_name("blackhole")
@inline(never)
@_semantics("optimize.sil.specialize.generic.never")
func blackhole<T>(_ t: T) {
  print(t) 
}
