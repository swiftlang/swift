// RUN: %target-swift-emit-silgen %s | %FileCheck %s

class C {}

protocol P {
    static func c(_: C) -> Self
}

enum E: P {
    case c(C)
}

// CHECK-LABEL: sil {{.*}} @$s19enum_witness_thunks1EOAA1PA2aDP1cyxAA1CCFZTW : $@convention(witness_method: P) (@guaranteed C, @thick E.Type) -> @out E
// CHECK:         [[COPY:%.*]] = copy_value
// CHECK:         apply {{.*}}([[COPY]]
