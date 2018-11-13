// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-silgen %s -swift-version 4 | %FileCheck %s

final class Final<T> {
    var x: T
    init(x: T) { self.x = x }
}
// CHECK-LABEL: final class Final<T> {
// CHECK:   @_hasStorage final var x: T { get set }
// CHECK:   init(x: T)
// CHECK:   deinit
// CHECK:   enum CodingKeys : CodingKey {
// CHECK:     case x
// CHECK:     var stringValue: String { get }
// CHECK:     init?(stringValue: String)
// CHECK:     var intValue: Int? { get }
// CHECK:     init?(intValue: Int)
// CHECK:     @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Final<T>.CodingKeys, _ b: Final<T>.CodingKeys) -> Bool
// CHECK:     var hashValue: Int { get }
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:   }
// CHECK: }

class Nonfinal<T> {
    var x: T
    init(x: T) { self.x = x }
}
// CHECK-LABEL: class Nonfinal<T> {
// CHECK:   @_hasStorage var x: T { get set }
// CHECK:   init(x: T)
// CHECK:   deinit
// CHECK:   enum CodingKeys : CodingKey {
// CHECK:     case x
// CHECK:     var stringValue: String { get }
// CHECK:     init?(stringValue: String)
// CHECK:     var intValue: Int? { get }
// CHECK:     init?(intValue: Int)
// CHECK:     @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Nonfinal<T>.CodingKeys, _ b: Nonfinal<T>.CodingKeys) -> Bool
// CHECK:     var hashValue: Int { get }
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:   }
// CHECK: }

// CHECK-LABEL: extension Final : Encodable where T : Encodable {
// CHECK:   func encode(to encoder: Encoder) throws
// CHECK: }
// CHECK-LABEL: extension Final : Decodable where T : Decodable {
// CHECK:   init(from decoder: Decoder) throws
// CHECK: }

// CHECK-LABEL: extension Nonfinal : Encodable where T : Encodable {
// CHECK:   func encode(to encoder: Encoder) throws
// CHECK: }

extension Final: Encodable where T: Encodable {}
// CHECK-LABEL: // Final<A>.encode(to:)
// CHECK-NEXT: sil hidden @$s29synthesized_conformance_class5FinalCAASERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Encodable> (@in_guaranteed Encoder, @guaranteed Final<T>) -> @error Error {

extension Final: Decodable where T: Decodable {}
// CHECK-LABEL: // Final<A>.init(from:)
// CHECK-NEXT: sil hidden @$s29synthesized_conformance_class5FinalCAASeRzlE4fromACyxGs7Decoder_p_tKcfC : $@convention(method) <T where T : Decodable> (@in Decoder, @thick Final<T>.Type) -> (@owned Final<T>, @error Error) {

extension Nonfinal: Encodable where T: Encodable {}
// CHECK-LABEL: // Nonfinal<A>.encode(to:)
// CHECK-NEXT: sil hidden @$s29synthesized_conformance_class8NonfinalCAASERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Encodable> (@in_guaranteed Encoder, @guaranteed Nonfinal<T>) -> @error Error {

// Witness tables for Final

// CHECK-LABEL: sil_witness_table hidden <T where T : Encodable> Final<T>: Encodable module synthesized_conformance_class {
// CHECK-NEXT:   method #Encodable.encode!1: <Self where Self : Encodable> (Self) -> (Encoder) throws -> () : @$s29synthesized_conformance_class5FinalCyxGSEAASERzlSE6encode2toys7Encoder_p_tKFTW	// protocol witness for Encodable.encode(to:) in conformance <A> Final<A>
// CHECK-NEXT:   conditional_conformance (T: Encodable): dependent
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden <T where T : Decodable> Final<T>: Decodable module synthesized_conformance_class {
// CHECK-NEXT:   method #Decodable.init!allocator.1: <Self where Self : Decodable> (Self.Type) -> (Decoder) throws -> Self : @$s29synthesized_conformance_class5FinalCyxGSeAASeRzlSe4fromxs7Decoder_p_tKcfCTW	// protocol witness for Decodable.init(from:) in conformance <A> Final<A>
// CHECK-NEXT:   conditional_conformance (T: Decodable): dependent
// CHECK-NEXT: }

// Witness tables for Nonfinal

// CHECK-LABEL: sil_witness_table hidden <T where T : Encodable> Nonfinal<T>: Encodable module synthesized_conformance_class {
// CHECK-NEXT:   method #Encodable.encode!1: <Self where Self : Encodable> (Self) -> (Encoder) throws -> () : @$s29synthesized_conformance_class8NonfinalCyxGSEAASERzlSE6encode2toys7Encoder_p_tKFTW	// protocol witness for Encodable.encode(to:) in conformance <A> Nonfinal<A>
// CHECK-NEXT:   conditional_conformance (T: Encodable): dependent
// CHECK-NEXT: }



