// RUN: %target-swift-frontend -emit-sil -module-name main -primary-file %s %S/Inputs/protocol-conformance-sr11018-other.swift -verify
// RUN: %target-swift-frontend -emit-sil -module-name main %s -primary-file %S/Inputs/protocol-conformance-sr11018-other.swift

func reproducer() -> Float { return Struct().func1(1.0) }
// expected-error@-1 {{cannot convert value of type 'Double' to expected argument type 'Struct.Input'}}
