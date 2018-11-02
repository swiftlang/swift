// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

class SR8083_Base: Codable {
  var thing: String { return "Abstract" }
}

class SR8083_Sub: SR8083_Base {
  override var thing: String { return "Yo" }
}

func sr8083(decoder: Decoder) throws {
  _ = try SR8083_Sub(from: decoder)
}

// CHECK-LABEL: sil_vtable SR8083_Base {
// CHECK-DAG: #SR8083_Base.init!initializer.1: (SR8083_Base.Type) -> () -> SR8083_Base : @$S23class_codable_inherited11SR8083_BaseCACycfc	// SR8083_Base.init()
// CHECK-DAG: #SR8083_Base.init!allocator.1: (SR8083_Base.Type) -> (Decoder) throws -> SR8083_Base : @$S23class_codable_inherited11SR8083_BaseC4fromACs7Decoder_p_tKcfC
// CHECK-DAG: #SR8083_Base.init!initializer.1: (SR8083_Base.Type) -> (Decoder) throws -> SR8083_Base : @$S23class_codable_inherited11SR8083_BaseC4fromACs7Decoder_p_tKcfc	// SR8083_Base.init(from:)
// CHECK: {{^}$}}

// CHECK-LABEL: sil_vtable SR8083_Sub {
// CHECK-DAG: #SR8083_Base.init!initializer.1: (SR8083_Base.Type) -> () -> SR8083_Base : @$S23class_codable_inherited10SR8083_SubCACycfc [override]	// SR8083_Sub.init()
// CHECK-DAG: #SR8083_Base.init!allocator.1: (SR8083_Base.Type) -> (Decoder) throws -> SR8083_Base : @$S23class_codable_inherited10SR8083_SubC4fromACs7Decoder_p_tKcfC [override]	// SR8083_Sub.__allocating_init(from:)
// CHECK-DAG: #SR8083_Base.init!initializer.1: (SR8083_Base.Type) -> (Decoder) throws -> SR8083_Base : @$S23class_codable_inherited10SR8083_SubC4fromACs7Decoder_p_tKcfc [override]	// SR8083_Sub.init(from:)
// CHECK: {{^}$}}
