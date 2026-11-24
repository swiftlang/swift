// RUN: %empty-directory(%t)
// RUN: %target-build-swift -O -wmo -parse-as-library -disable-cmo -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module -DMODULE %s -c -o %t/module.o
// TODO: ARCSequenceOpts miscompiles, therefore disabled. With OSSA, this optimization will go away soon, anyway.
// RUN: %target-build-swift -O -wmo -module-name=Main -I%t %s -emit-sil -Xllvm -sil-disable-pass=arc-sequence-opts | %FileCheck %s

// REQUIRES: optimized_stdlib

#if MODULE

public struct S {
  public let s: String
  public init(_ s: String) { self.s = s }
}

public let globalLetArray = [S("a"), S("b")]
public let globalLetStruct = S("c")
public var globalVarArray = [S("a"), S("b")]

public func consumeSequence<T: Sequence>(_ t: T) where T.Element == S {
  print(t)
}

public func consume(_ s: S) {
  print(s)
}

#else

import Module

// CHECK-LABEL: sil @$s4Main19testGenericLetArrayyyF :
// CHECK:         [[A:%.*]] = pointer_to_address
// CHECK-NOT:     retain
// CHECK:         apply {{%.*}}<Array<S>>([[A]])
// CHECK-NOT:     release
// CHECK:       } // end sil function '$s4Main19testGenericLetArrayyyF'
public func testGenericLetArray() {
  consumeSequence(globalLetArray)
}

// CHECK-LABEL: sil @$s4Main13testLetStructyyF :
// CHECK-NOT:     retain
// CHECK:         apply
// CHECK-NOT:     release
// CHECK:       } // end sil function '$s4Main13testLetStructyyF'
public func testLetStruct() {
  consume(globalLetStruct)
}

// CHECK-LABEL: sil @$s4Main19testGenericVarArrayyyF :
// CHECK:         retain_value
// CHECK:         apply
// CHECK:         release_value
// CHECK:       } // end sil function '$s4Main19testGenericVarArrayyyF'
public func testGenericVarArray() {
  consumeSequence(globalVarArray)
}

#endif
