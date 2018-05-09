// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s -swift-version 4 | %FileCheck %s

struct Struct<T> {
    var x: T
}

extension Struct: Equatable where T: Equatable {}
extension Struct: Hashable where T: Hashable {}
extension Struct: Codable where T: Codable {}

enum Enum<T> {
    case a(T), b(T)
}

extension Enum: Equatable where T: Equatable {}
extension Enum: Hashable where T: Hashable {}

final class Final<T> {
    var x: T
    init(x: T) { self.x = x }
}

extension Final: Encodable where T: Encodable {}
extension Final: Decodable where T: Decodable {}

class Nonfinal<T> {
    var x: T
    init(x: T) { self.x = x }
}
extension Nonfinal: Encodable where T: Encodable {}

func doEquality<T: Equatable>(_: T) {}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S23synthesized_conformance8equalityyyF"()
public func equality() {
    // CHECK: [[Struct_Equatable:%.*]] = call i8** @"$S23synthesized_conformance6StructVySiGACyxGs9EquatableAAsAFRzlWl"()
    // CHECK-NEXT: call swiftcc void @"$S23synthesized_conformance10doEqualityyyxs9EquatableRzlF"(%swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[Struct_Equatable]])
    doEquality(Struct(x: 1))
    // CHECK: [[Enum_Equatable:%.*]] = call i8** @"$S23synthesized_conformance4EnumOySiGACyxGs9EquatableAAsAFRzlWl"()
    // CHECK-NEXT: call swiftcc void @"$S23synthesized_conformance10doEqualityyyxs9EquatableRzlF"(%swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[Enum_Equatable]])
    doEquality(Enum.a(1))
}

func doEncodable<T: Encodable>(_: T) {}
// CHECK-LABEL: define{{( protected)?}} swiftcc void @"$S23synthesized_conformance9encodableyyF"()
public func encodable() {
    // CHECK: [[Struct_Encodable:%.*]] = call i8** @"$S23synthesized_conformance6StructVySiGACyxGs9EncodableAAs9DecodableRzsAFRzlWl"()
    // CHECK-NEXT: call swiftcc void @"$S23synthesized_conformance11doEncodableyyxs0D0RzlF"(%swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[Struct_Encodable]])
    doEncodable(Struct(x: 1))
    // CHECK: [[Final_Encodable:%.*]] = call i8** @"$S23synthesized_conformance5FinalCySiGACyxGs9EncodableAAsAFRzlWl"()
    // CHECK-NEXT: call swiftcc void @"$S23synthesized_conformance11doEncodableyyxs0D0RzlF"(%swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[Final_Encodable]])
    doEncodable(Final(x: 1))
    // CHECK: [[Nonfinal_Encodable:%.*]] = call i8** @"$S23synthesized_conformance8NonfinalCySiGACyxGs9EncodableAAsAFRzlWl"()
    // CHECK-NEXT: call swiftcc void @"$S23synthesized_conformance11doEncodableyyxs0D0RzlF"(%swift.opaque* noalias nocapture {{%.*}}, %swift.type* {{%.*}}, i8** [[Nonfinal_Encodable]])
    doEncodable(Nonfinal(x: 1))
}
