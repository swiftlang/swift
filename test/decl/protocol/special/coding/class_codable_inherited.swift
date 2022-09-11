// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

// https://github.com/apple/swift/issues/50616

class Base: Codable {
  var thing: String { return "Abstract" }
}

class Sub: Base {
  override var thing: String { return "Yo" }
}

func f(decoder: Decoder) throws {
  _ = try Sub(from: decoder)
}

// CHECK-LABEL: sil_vtable Base {
// CHECK-DAG: #Base.init!allocator: (Base.Type) -> () -> Base : @$s23class_codable_inherited4BaseCACycfC
// CHECK-DAG: #Base.init!allocator: (Base.Type) -> (any Decoder) throws -> Base : @$s23class_codable_inherited4BaseC4fromACs7Decoder_p_tKcfC
// CHECK: {{^}$}}

// CHECK-LABEL: sil_vtable Sub {
// CHECK-DAG: #Base.init!allocator: (Base.Type) -> () -> Base : @$s23class_codable_inherited3SubCACycfC [override]
// CHECK-DAG: #Base.init!allocator: (Base.Type) -> (any Decoder) throws -> Base : @$s23class_codable_inherited3SubC4fromACs7Decoder_p_tKcfC [override]	// Sub.__allocating_init(from:)
// CHECK: {{^}$}}
