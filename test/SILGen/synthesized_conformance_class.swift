// RUN: %target-swift-frontend -emit-silgen %s -swift-version 4 | %FileCheck %s

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
// CHECK:     @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Final<T>.CodingKeys, _ b: Final<T>.CodingKeys) -> Bool
// CHECK:     var hashValue: Int { get }
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:     var stringValue: String { get }
// CHECK:     init?(stringValue: String)
// CHECK:     var intValue: Int? { get }
// CHECK:     init?(intValue: Int)
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
// CHECK:     @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Nonfinal<T>.CodingKeys, _ b: Nonfinal<T>.CodingKeys) -> Bool
// CHECK:     var hashValue: Int { get }
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:     var stringValue: String { get }
// CHECK:     init?(stringValue: String)
// CHECK:     var intValue: Int? { get }
// CHECK:     init?(intValue: Int)
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
// CHECK-NEXT: sil hidden [ossa] @$s29synthesized_conformance_class5FinalCAASERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Encodable> (@in_guaranteed Encoder, @guaranteed Final<T>) -> @error Error {

extension Final: Decodable where T: Decodable {}
// CHECK-LABEL: // Final<A>.init(from:)
// CHECK-NEXT: sil hidden [exact_self_class] [ossa] @$s29synthesized_conformance_class5FinalCAASeRzlE4fromACyxGs7Decoder_p_tKcfC : $@convention(method) <T where T : Decodable> (@in Decoder, @thick Final<T>.Type) -> (@owned Final<T>, @error Error) {

extension Nonfinal: Encodable where T: Encodable {}
// CHECK-LABEL: // Nonfinal<A>.encode(to:)
// CHECK-NEXT: sil hidden [ossa] @$s29synthesized_conformance_class8NonfinalCAASERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Encodable> (@in_guaranteed Encoder, @guaranteed Nonfinal<T>) -> @error Error {

final class FinalHashableClass : Hashable {
  static func ==(lhs: FinalHashableClass, rhs: FinalHashableClass) -> Bool {
    return false
  }

  func hash(into: inout Hasher) {}
}

// CHECK-LABEL: sil hidden [ossa] @$s29synthesized_conformance_class4doItySiAA18FinalHashableClassCF : $@convention(thin) (@guaranteed FinalHashableClass) -> Int {
// CHECK: bb0(%0 : @guaranteed $FinalHashableClass):
// CHECK:   [[FN:%.*]] = function_ref @$s29synthesized_conformance_class18FinalHashableClassC9hashValueSivg : $@convention(method) (@guaranteed FinalHashableClass) -> Int
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]](%0) : $@convention(method) (@guaranteed FinalHashableClass) -> Int
// CHECK-NEXT: return [[RESULT]] : $Int

func doIt(_ c: FinalHashableClass) -> Int {
  return c.hashValue
}

// VTable for FinalHashableClass
//
// Note: we should not be emitting a vtable entry for the synthesized
// FinalHashableClass.hashValue getter!

// CHECK: sil_vtable FinalHashableClass {
// CHECK-NEXT: #FinalHashableClass.init!allocator.1: (FinalHashableClass.Type) -> () -> FinalHashableClass : @$s29synthesized_conformance_class18FinalHashableClassCACycfC
// CHECK-NEXT: #FinalHashableClass.deinit!deallocator.1: @$s29synthesized_conformance_class18FinalHashableClassCfD
// CHECK-NEXT: }

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



