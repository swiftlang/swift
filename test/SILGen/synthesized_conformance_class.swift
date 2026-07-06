// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -swift-version 4 | %FileCheck %s

final class Final<T> {
    var x: T
    init(x: T) { self.x = x }
}

// CHECK-LABEL: final class Final<T> {
// CHECK:   @_hasStorage final var x: T { get set }
// CHECK:   init(x: T)
// CHECK:   enum CodingKeys : CodingKey {
// CHECK:     case x
// CHECK:     init?(stringValue: String)
// CHECK:     init?(intValue: Int)
// CHECK:     @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Final<T>.CodingKeys, _ b: Final<T>.CodingKeys) -> Bool
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:     var hashValue: Int { get }
// CHECK:     var intValue: Int? { get }
// CHECK:     var stringValue: String { get }
// CHECK:   }
// CHECK:   deinit
// CHECK: }

class Nonfinal<T> {
    var x: T
    init(x: T) { self.x = x }
}
// CHECK-LABEL: class Nonfinal<T> {
// CHECK:   @_hasStorage var x: T { get set }
// CHECK:   init(x: T)
// CHECK:   enum CodingKeys : CodingKey {
// CHECK:     case x
// CHECK:     init?(stringValue: String)
// CHECK:     init?(intValue: Int)
// CHECK:     @_implements(Equatable, ==(_:_:)) static func __derived_enum_equals(_ a: Nonfinal<T>.CodingKeys, _ b: Nonfinal<T>.CodingKeys) -> Bool
// CHECK:     func hash(into hasher: inout Hasher)
// CHECK:     var hashValue: Int { get }
// CHECK:     var intValue: Int? { get }
// CHECK:     var stringValue: String { get }
// CHECK:   }
// CHECK:   deinit
// CHECK: }

// CHECK-LABEL: extension Final : Encodable where T : Encodable {
// CHECK:   func encode(to encoder: any Encoder) throws
// CHECK: }
// CHECK-LABEL: extension Final : Decodable where T : Decodable {
// CHECK:   init(from decoder: any Decoder) throws
// CHECK: }

// CHECK-LABEL: extension Nonfinal : Encodable where T : Encodable {
// CHECK:   func encode(to encoder: any Encoder) throws
// CHECK: }

// Make sure that CodingKeys members are actually emitted.

// CHECK-LABEL: sil private [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}11stringValueAFyx_GSgSS_tcfC : $@convention(method) <T> (@owned String, @thin Final<T>.CodingKeys.Type) -> Optional<Final<T>.CodingKeys> {
// CHECK-LABEL: sil private [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}8intValueAFyx_GSgSi_tcfC : $@convention(method) <T> (Int, @thin Final<T>.CodingKeys.Type) -> Optional<Final<T>.CodingKeys> {
// CHECK-LABEL: sil private [_semantics "derived_enum_equals"] [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}21__derived_enum_equalsySbAFyx_G_AHtFZ : $@convention(method) <T> (Final<T>.CodingKeys, Final<T>.CodingKeys, @thin Final<T>.CodingKeys.Type) -> Bool {
// CHECK-LABEL: sil private [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}4hash4intoys6HasherVz_tF : $@convention(method) <T> (@inout Hasher, Final<T>.CodingKeys) -> () {
// CHECK-LABEL: sil private [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}9hashValueSivg : $@convention(method) <T> (Final<T>.CodingKeys) -> Int {
// CHECK-LABEL: sil private [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}8intValueSiSgvg : $@convention(method) <T> (Final<T>.CodingKeys) -> Optional<Int> {
// CHECK-LABEL: sil private [ossa] @$s29synthesized_conformance_class5FinalC10CodingKeys{{.*}}11stringValueSSvg : $@convention(method) <T> (Final<T>.CodingKeys) -> @owned String {

extension Final: Encodable where T: Encodable {}
// CHECK-LABEL: // Final<A>.encode(to:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s29synthesized_conformance_class5FinalCAASERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Encodable> (@in_guaranteed any Encoder, @guaranteed Final<T>) -> @error any Error {

extension Final: Decodable where T: Decodable {}
// CHECK-LABEL: // Final<A>.init(from:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [exact_self_class] [ossa] @$s29synthesized_conformance_class5FinalCAASeRzlE4fromACyxGs7Decoder_p_tKcfC : $@convention(method) <T where T : Decodable> (@in any Decoder, @thick Final<T>.Type) -> (@owned Final<T>, @error any Error) {

extension Nonfinal: Encodable where T: Encodable {}
// CHECK-LABEL: // Nonfinal<A>.encode(to:)
// CHECK-NEXT: // Isolation: unspecified
// CHECK-NEXT: sil hidden [ossa] @$s29synthesized_conformance_class8NonfinalCAASERzlE6encode2toys7Encoder_p_tKF : $@convention(method) <T where T : Encodable> (@in_guaranteed any Encoder, @guaranteed Nonfinal<T>) -> @error any Error {

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
// CHECK-NEXT: #FinalHashableClass.init!allocator: (FinalHashableClass.Type) -> () -> FinalHashableClass : @$s29synthesized_conformance_class18FinalHashableClassCACycfC
// CHECK-NEXT: #FinalHashableClass.deinit!deallocator: @$s29synthesized_conformance_class18FinalHashableClassCfD
// CHECK-NEXT: }

// Witness tables for Final

// CHECK-LABEL: sil_witness_table hidden <T where T : Encodable> Final<T>: Encodable module synthesized_conformance_class {
// CHECK-NEXT:   method #Encodable.encode: <Self where Self : Encodable> (Self) -> (any Encoder) throws -> () : @$s29synthesized_conformance_class5FinalCyxGSEAASERzlSE6encode2toys7Encoder_p_tKFTW	// protocol witness for Encodable.encode(to:) in conformance <A> Final<A>
// CHECK-NEXT:   conditional_conformance (T: Encodable): dependent
// CHECK-NEXT: }

// CHECK-LABEL: sil_witness_table hidden <T where T : Decodable> Final<T>: Decodable module synthesized_conformance_class {
// CHECK-NEXT:   method #Decodable.init!allocator: <Self where Self : Decodable> (Self.Type) -> (any Decoder) throws -> Self : @$s29synthesized_conformance_class5FinalCyxGSeAASeRzlSe4fromxs7Decoder_p_tKcfCTW	// protocol witness for Decodable.init(from:) in conformance <A> Final<A>
// CHECK-NEXT:   conditional_conformance (T: Decodable): dependent
// CHECK-NEXT: }

// Witness tables for Nonfinal

// CHECK-LABEL: sil_witness_table hidden <T where T : Encodable> Nonfinal<T>: Encodable module synthesized_conformance_class {
// CHECK-NEXT:   method #Encodable.encode: <Self where Self : Encodable> (Self) -> (any Encoder) throws -> () : @$s29synthesized_conformance_class8NonfinalCyxGSEAASERzlSE6encode2toys7Encoder_p_tKFTW	// protocol witness for Encodable.encode(to:) in conformance <A> Nonfinal<A>
// CHECK-NEXT:   conditional_conformance (T: Encodable): dependent
// CHECK-NEXT: }



