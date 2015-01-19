// RUN: %target-swift-frontend -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | FileCheck %s

// REQUIRES: CPU=x86_64

protocol A { func a() }
protocol B { func b() }
protocol C : class { func c() }
@objc protocol O { func o() }
@objc protocol OPT { 
  optional func opt()
  optional static func static_opt()

  optional var prop: O { get }
  optional subscript (x: O) -> O { get }
}

protocol AB : A, B { func ab() }
protocol ABO : A, B, O { func abo() }

// CHECK: @_TMp17protocol_metadata1A = constant %swift.protocol {
// -- size 72
// -- flags: 1 = Swift | 2 = Not Class-Constrained | 4 = Needs Witness Table
// CHECK:   i32 72, i32 7
// CHECK: }

// CHECK: @_TMp17protocol_metadata1B = constant %swift.protocol {
// CHECK:   i32 72, i32 7
// CHECK: }

// CHECK: @_TMp17protocol_metadata1C = constant %swift.protocol {
// -- flags: 1 = Swift | 4 = Needs Witness Table
// CHECK:   i32 72, i32 5
// CHECK: }

// -- @objc protocol O uses ObjC symbol mangling and layout
// CHECK: @_PROTOCOL__TtP17protocol_metadata1O_ = private constant { {{.*}} i32, { [1 x i8*] }* } {
// CHECK:   @_PROTOCOL_INSTANCE_METHODS__TtP17protocol_metadata1O_,
// -- flags: 1 = Swift
// CHECK:   i32 80, i32 1
// CHECK: @_PROTOCOL_METHOD_TYPES__TtP17protocol_metadata1O_
// CHECK: }

// -- @objc protocol OPT uses ObjC symbol mangling and layout
// CHECK: @_PROTOCOL__TtP17protocol_metadata3OPT_ = private constant { {{.*}} i32, { [4 x i8*] }* } {
// CHECK:   @_PROTOCOL_INSTANCE_METHODS_OPT__TtP17protocol_metadata3OPT_,
// CHECK:   @_PROTOCOL_CLASS_METHODS_OPT__TtP17protocol_metadata3OPT_,
// CHECK:   i32 80, i32 1
// CHECK:   @_PROTOCOL_METHOD_TYPES__TtP17protocol_metadata3OPT_
// -- flags: 1 = Swift
// CHECK: }

// -- inheritance lists for refined protocols

// CHECK: [[AB_INHERITED:@.*]] = private constant { {{.*}}* } {
// CHECK:   i64 2,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1A,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1B
// CHECK: }
// CHECK: @_TMp17protocol_metadata2AB = constant %swift.protocol { 
// CHECK:   [[AB_INHERITED]]
// CHECK:   i32 72, i32 7
// CHECK: }

// CHECK: [[ABO_INHERITED:@.*]] = private constant { {{.*}}* } {
// CHECK:   i64 3,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1A,
// CHECK:   %swift.protocol* @_TMp17protocol_metadata1B,
// CHECK:   {{.*}}* @_PROTOCOL__TtP17protocol_metadata1O_
// CHECK: }

func reify_metadata<T>(x: T) {}

// CHECK: define hidden void @_TF17protocol_metadata14protocol_types
func protocol_types(a: A,
                    abc: protocol<A, B, C>,
                    abco: protocol<A, B, C, O>) {
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1A
  // CHECK: call %swift.type* @swift_getExistentialTypeMetadata(i64 1, %swift.protocol** {{%.*}})
  reify_metadata(a)
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1A
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1B
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1C
  // CHECK: call %swift.type* @swift_getExistentialTypeMetadata(i64 3, %swift.protocol** {{%.*}})
  reify_metadata(abc)
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1A
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1B
  // CHECK: store %swift.protocol* @_TMp17protocol_metadata1C
  // CHECK: [[O_REF:%.*]] = load i8** @"\01l_OBJC_PROTOCOL_REFERENCE_$__TtP17protocol_metadata1O_"
  // CHECK: [[O_REF_BITCAST:%.*]] = bitcast i8* [[O_REF]] to %swift.protocol*
  // CHECK: store %swift.protocol* [[O_REF_BITCAST]]
  // CHECK: call %swift.type* @swift_getExistentialTypeMetadata(i64 4, %swift.protocol** {{%.*}})
  reify_metadata(abco)
}

