// RUN: %target-swift-emit-ir %s -I %S/Inputs -enable-cxx-interop -validate-tbd-against-ir=none -disable-llvm-verify | %FileCheck %s

import POD

// TODO: this should not be opaque.
// CHECK: %TSo7IntPairV = type opaque
// CHECK: %struct.IntPair = type { i32, i32 }

// CHECK-LABEL: define swiftcc void @"$s4main4testyyF"

// CHECK: [[X:%.*]] = alloca %TSo7IntPairV*
// CHECK: [[TMP:%.*]] = alloca %TSo7IntPairV*

// CHECK: [[CREATED:%.*]] = call %struct.IntPair* @_ZN7IntPair6createEv()
// CHECK: [[SWIFT_CREATED:%.*]] = bitcast %struct.IntPair* [[CREATED]] to %TSo7IntPairV*
// CHECK: store %TSo7IntPairV* [[SWIFT_CREATED]], %TSo7IntPairV** [[X]]
// CHECK: store %TSo7IntPairV* [[SWIFT_CREATED]], %TSo7IntPairV** [[TMP]]

// CHECK: [[TMP_LOAD:%.*]] = load %TSo7IntPairV*, %TSo7IntPairV** [[TMP]]
// CHECK: [[CLANG_CREATED:%.*]] = bitcast %TSo7IntPairV* [[TMP_LOAD]] to %struct.IntPair*
// CHECK: call i32 @_ZNK7IntPair4testEv(%struct.IntPair* [[CLANG_CREATED]])

// CHECK: ret void

public func test() {
  var x = IntPair.create()
  _ = x.test()
}
