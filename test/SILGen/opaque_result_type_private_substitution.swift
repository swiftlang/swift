// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module %S/Inputs/opaque_result_type_private_substitution_other.swift -emit-module-path %t/opaque_result_type_private_substitution_other.swiftmodule
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-silgen-ossa -o /dev/null -sil-verify-all -enable-sil-opaque-values %s -I %t -DMODULE
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-silgen %s -I %t -DMODULE

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-module %S/Inputs/opaque_result_type_private_substitution_other.swift -emit-module-path %t/opaque_result_type_private_substitution_other.swiftmodule -enable-library-evolution
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-silgen %s -I %t -DMODULE

// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-silgen %s %S/Inputs/opaque_result_type_private_substitution_other.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-silgen -primary-file %s %S/Inputs/opaque_result_type_private_substitution_other.swift
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -emit-silgen -primary-file %s -primary-file %S/Inputs/opaque_result_type_private_substitution_other.swift

#if MODULE
import opaque_result_type_private_substitution_other
#endif

struct S1: P {
  var v: some P { S2().foo() }
}

private struct S2: P {
  var v: some P { S() }
}
