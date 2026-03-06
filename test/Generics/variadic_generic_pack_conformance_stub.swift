// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// Ensure protocol conformance diagnostics don't crash when requirements
// involve pack expansion types. The ASTPrinter's type substitution via
// TypeTransformContext crashes on pack expansions during stub generation.

// ---- Pack expansion in function signature (metatype packs) ----

protocol PackMetatypeProto1 {
  func f<each T>(_: (repeat (each T).Type)) -> (repeat (each T).Type)
  // expected-note@-1 {{protocol requires function 'f' with type .*}}
}

class ClassMetatype: PackMetatypeProto1 {} // expected-error {{type 'ClassMetatype' does not conform to protocol 'PackMetatypeProto1'}}

protocol PackMetatypeProto2 {
  func f<each T>(_: (repeat (each T).Type)) -> (repeat (each T).Type)
  // expected-note@-1 {{protocol requires function 'f' with type .*}}
}

struct StructMetatype: PackMetatypeProto2 {} // expected-error {{type 'StructMetatype' does not conform to protocol 'PackMetatypeProto2'}}

protocol PackMetatypeProto3 {
  func f<each T>(_: (repeat (each T).Type)) -> (repeat (each T).Type)
  // expected-note@-1 {{protocol requires function 'f' with type .*}}
}

enum EnumMetatype: PackMetatypeProto3 {} // expected-error {{type 'EnumMetatype' does not conform to protocol 'PackMetatypeProto3'}}

// ---- Pack expansion with associated type constraints ----

protocol HasAssoc { associatedtype U }

protocol PackAssocProto1 {
  func g<each T: HasAssoc>(_: (repeat (each T))) -> (repeat (each T).U)
  // expected-note@-1 {{protocol requires function 'g' with type .*}}
}

class ClassAssoc: PackAssocProto1 {} // expected-error {{type 'ClassAssoc' does not conform to protocol 'PackAssocProto1'}}

protocol PackAssocProto2 {
  func g<each T: HasAssoc>(_: (repeat (each T))) -> (repeat (each T).U)
  // expected-note@-1 {{protocol requires function 'g' with type .*}}
}

struct StructAssoc: PackAssocProto2 {} // expected-error {{type 'StructAssoc' does not conform to protocol 'PackAssocProto2'}}

protocol PackAssocProto3 {
  func g<each T: HasAssoc>(_: (repeat (each T))) -> (repeat (each T).U)
  // expected-note@-1 {{protocol requires function 'g' with type .*}}
}

enum EnumAssoc: PackAssocProto3 {} // expected-error {{type 'EnumAssoc' does not conform to protocol 'PackAssocProto3'}}

// ---- Valid conformance with pack expansion types should still compile ----

protocol ValidPackProto {
  func f<each T>(_: (repeat (each T).Type)) -> (repeat (each T).Type)
}

struct ValidConformer: ValidPackProto {
  func f<each T>(_ t: (repeat (each T).Type)) -> (repeat (each T).Type) {
    return t
  }
}
