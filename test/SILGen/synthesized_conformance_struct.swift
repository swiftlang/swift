// RUN: %target-swift-frontend -emit-silgen %s -swift-version 4 | %FileCheck -check-prefix CHECK -check-prefix CHECK-FRAGILE %s
// RUN: %target-swift-frontend -emit-silgen %s -swift-version 4 -enable-library-evolution | %FileCheck -check-prefix CHECK -check-prefix CHECK-RESILIENT %s

struct Struct<T> {
    var x: T
}

// CHECK-LABEL: struct Struct<T> {
// CHECK:   @_hasStorage var x: T { get set }
// CHECK:   enum CodingKeys : CodingKey {
// CHECK:     case x
// CHECK:     init?(stringValue: String)
// CHECK:     init?(intValue: Int)
// CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Struct<T>.CodingKeys, _ b: Struct<T>.CodingKeys) -> Bool
// CHECK-RESILIENT: static func == (a: Struct<T>.CodingKeys, b: Struct<T>.CodingKeys) -> Bool
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:     var hashValue: Int { get }
// CHECK:     var intValue: Int? { get }
// CHECK:     var stringValue: String { get }
// CHECK:   }
// CHECK:   init(x: T)
// CHECK: }
// CHECK-LABEL: extension Struct : Equatable where T : Equatable {
// CHECK-FRAGILE:   @_implements(Equatable, ==(_:_:)) static func __derived_struct_equals(_ a: Struct<T>, _ b: Struct<T>) -> Bool
// CHECK-RESILIENT: static func == (a: Struct<T>, b: Struct<T>) -> Bool
// CHECK: }
// CHECK-LABEL: extension Struct : Hashable where T : Hashable {
// CHECK:   func hash(into hasher: inout Hasher)
// CHECK:   var hashValue: Int { get }
// CHECK: }
// CHECK-LABEL: extension Struct : Decodable & Encodable where T : Decodable, T : Encodable {
// CHECK:   func encode(to encoder: any Encoder) throws
// CHECK:   init(from decoder: any Decoder) throws
// CHECK: }

extension Struct: Equatable where T: Equatable {}
// CHECK-FRAGILE-LABEL: // static Struct<A>.__derived_struct_equals(_:_:)
// CHECK-FRAGILE-NEXT: // Isolation: unspecified
// CHECK-FRAGILE-NEXT: sil hidden [ossa] @$s30synthesized_conformance_struct6StructVAASQRzlE010__derived_C7_equalsySbACyxG_AEtFZ : $@convention(method) <T where T : Equatable> (@in_guaranteed Struct<T>, @in_guaranteed Struct<T>, @thin Struct<T>.Type) -> Bool {
// CHECK-RESILIENT-LABEL: // static Struct<A>.== infix(_:_:)
// CHECK-RESILIENT-NEXT: // Isolation: unspecified
// CHECK-RESILIENT-NEXT: sil hidden [ossa] @$s30synthesized_conformance_struct6StructVAASQRzlE2eeoiySbACyxG_AEtFZ : $@convention(method) <T where T : Equatable> (@in_guaranteed Struct<T>, @in_guaranteed Struct<T>, @thin Struct<T>.Type) -> Bool {

extension Struct: Hashable where T: Hashable {}
// CHECK-LABEL: // Struct<A>.hash(into:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s30synthesized_conformance_struct6StructVAASHRzlE4hash4intoys6HasherVz_tF : $@convention(method) <T where T : Hashable> (@inout Hasher, @in_guaranteed Struct<T>) -> () {

// CHECK-LABEL: // Struct<A>.hashValue.getter
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s30synthesized_conformance_struct6StructVAASHRzlE9hashValueSivg : $@convention(method) <T where T : Hashable> (@in_guaranteed Struct<T>) -> Int {

extension Struct: Codable where T: Codable {}
// CHECK-LABEL: // Struct<A>.encode(to:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s30synthesized_conformance_struct6StructVAASeRzSERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Decodable, T : Encodable> (@in_guaranteed any Encoder, @in_guaranteed Struct<T>) -> @error any Error {

// CHECK-LABEL: // Struct<A>.init(from:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s30synthesized_conformance_struct6StructVAASeRzSERzlE4fromACyxGs7Decoder_p_tKcfC : $@convention(method) <T where T : Decodable, T : Encodable> (@in any Decoder, @thin Struct<T>.Type) -> (@out Struct<T>, @error any Error)


// Witness tables

// CHECK-LABEL: sil_witness_table hidden <T where T : Equatable> Struct<T>: Equatable module synthesized_conformance_struct {
// CHECK-NEXT:   method #Equatable."==": <Self where Self : Equatable> (Self.Type) -> (Self, Self) -> Bool : @$s30synthesized_conformance_struct6StructVyxGSQAASQRzlSQ2eeoiySbx_xtFZTW	// protocol witness for static Equatable.== infix(_:_:) in conformance <A> Struct<A>
// CHECK-NEXT:   conditional_conformance (T: Equatable): dependent
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden <T where T : Hashable> Struct<T>: Hashable module synthesized_conformance_struct {
// CHECK-DAG:   base_protocol Equatable: <T where T : Equatable> Struct<T>: Equatable module synthesized_conformance_struct
// CHECK-DAG:   method #Hashable.hashValue!getter: <Self where Self : Hashable> (Self) -> () -> Int : @$s30synthesized_conformance_struct6StructVyxGSHAASHRzlSH9hashValueSivgTW	// protocol witness for Hashable.hashValue.getter in conformance <A> Struct<A>
// CHECK-DAG:   method #Hashable.hash: <Self where Self : Hashable> (Self) -> (inout Hasher) -> () : @$s30synthesized_conformance_struct6StructVyxGSHAASHRzlSH4hash4intoys6HasherVz_tFTW	// protocol witness for Hashable.hash(into:) in conformance <A> Struct<A>
// CHECK-DAG:   method #Hashable._rawHashValue: <Self where Self : Hashable> (Self) -> (Int) -> Int : @$s30synthesized_conformance_struct6StructVyxGSHAASHRzlSH13_rawHashValue4seedS2i_tFTW // protocol witness for Hashable._rawHashValue(seed:) in conformance <A> Struct<A>
// CHECK-DAG:   conditional_conformance (T: Hashable): dependent
// CHECK: }

// CHECK-LABEL: sil_witness_table hidden <T where T : Decodable, T : Encodable> Struct<T>: Decodable module synthesized_conformance_struct {
// CHECK-NEXT:   method #Decodable.init!allocator: <Self where Self : Decodable> (Self.Type) -> (any Decoder) throws -> Self : @$s30synthesized_conformance_struct6StructVyxGSeAASeRzSERzlSe4fromxs7Decoder_p_tKcfCTW	// protocol witness for Decodable.init(from:) in conformance <A> Struct<A>
// CHECK-NEXT:   conditional_conformance (T: Decodable): dependent
// CHECK-NEXT:   conditional_conformance (T: Encodable): dependent
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden <T where T : Decodable, T : Encodable> Struct<T>: Encodable module synthesized_conformance_struct {
// CHECK-NEXT:   method #Encodable.encode: <Self where Self : Encodable> (Self) -> (any Encoder) throws -> () : @$s30synthesized_conformance_struct6StructVyxGSEAASeRzSERzlSE6encode2toys7Encoder_p_tKFTW	// protocol witness for Encodable.encode(to:) in conformance <A> Struct<A>
// CHECK-NEXT:   conditional_conformance (T: Decodable): dependent
// CHECK-NEXT:   conditional_conformance (T: Encodable): dependent
// CHECK-NEXT: }
