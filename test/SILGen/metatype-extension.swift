// REQUIRES: swift_feature_ProtocolMetatypeExtensions
// RUN: %target-swift-emit-silgen -enable-experimental-feature ProtocolMetatypeExtensions %s -module-name metatype_extension | %FileCheck %s

protocol P {}

extension P.Protocol {
  var value: Int { 42 }
  func greet() -> String { "hello" }
}

// Members are non-generic: no type parameters, no witness tables.

// CHECK-LABEL: sil hidden [ossa] @$s18metatype_extension1PPAAE5valueSivg : $@convention(method) (@thin (any P).Type) -> Int
// CHECK-NOT:     witness_method
// CHECK-NOT:     witness_table
// CHECK:       } // end sil function '$s18metatype_extension1PPAAE5valueSivg'

// CHECK-LABEL: sil hidden [ossa] @$s18metatype_extension1PPAAE5greetSSyF : $@convention(method) (@thin (any P).Type) -> @owned String
// CHECK-NOT:     witness_method
// CHECK-NOT:     witness_table
// CHECK:       } // end sil function '$s18metatype_extension1PPAAE5greetSSyF'

// Direct access on the protocol metatype.

// CHECK-LABEL: sil hidden [ossa] @$s18metatype_extension18testMetatypeDirectyyF
// CHECK:         [[META:%[0-9]+]] = metatype $@thin (any P).Type
// CHECK:         [[FN:%[0-9]+]] = function_ref @$s18metatype_extension1PPAAE5valueSivg : $@convention(method) (@thin (any P).Type) -> Int
// CHECK:         apply [[FN]]([[META]])
// CHECK:       } // end sil function '$s18metatype_extension18testMetatypeDirectyyF'
func testMetatypeDirect() {
  let _ = P.value
}

// Store the protocol metatype and dispatch through the stored value.

// CHECK-LABEL: sil hidden [ossa] @$s18metatype_extension18testMetatypeStoredyyF
// CHECK:         [[META:%[0-9]+]] = metatype $@thin (any P).Type
// CHECK:         [[FN:%[0-9]+]] = function_ref @$s18metatype_extension1PPAAE5greetSSyF : $@convention(method) (@thin (any P).Type) -> @owned String
// CHECK:         apply [[FN]]([[META]])
// CHECK:       } // end sil function '$s18metatype_extension18testMetatypeStoredyyF'
func testMetatypeStored() {
  let _ = P.self.greet()
}

// Protocol refinement: members do not propagate.

protocol Q: P {}

extension Q.Protocol {
  var qValue: Int { 99 }
}

// CHECK-LABEL: sil hidden [ossa] @$s18metatype_extension22testRefinementMetatypeyyF
// CHECK:         function_ref @$s18metatype_extension1QPAAE6qValueSivg
// CHECK-NOT:     function_ref {{.*}}PPAAE5value
// CHECK:       } // end sil function '$s18metatype_extension22testRefinementMetatypeyyF'
func testRefinementMetatype() {
  let _ = Q.qValue
}
