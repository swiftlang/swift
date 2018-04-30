// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: CPU=x86_64

protocol A { func a() }
protocol B { func b() }
protocol C : class { func c() }
@objc protocol O { func o() }
@objc protocol OPT { 
  @objc optional func opt()
  @objc optional static func static_opt()

  @objc optional var prop: O { get }
  @objc optional subscript (x: O) -> O { get }
}

protocol AB : A, B { func ab() }
protocol ABO : A, B, O { func abo() }

// CHECK: @"$S17protocol_metadata1AMp" = hidden constant %swift.protocol {
// -- size 72
// -- flags: 1 = Swift | 2 = Not Class-Constrained | 4 = Needs Witness Table
// CHECK-SAME:   i32 72, i32 7,
// CHECK-SAME:   i32 1,
// CHECK-SAME:   i32 trunc (i64 sub (i64 ptrtoint ([1 x %swift.protocol_requirement]* [[A_REQTS:@".*"]] to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.protocol, %swift.protocol* @"$S17protocol_metadata1AMp", i32 0, i32 11) to i64)) to i32)
// CHECK-SAME: }

// CHECK: @"$S17protocol_metadata1BMp" = hidden constant %swift.protocol {
// CHECK-SAME:   i32 72, i32 7,
// CHECK-SAME:   i32 1,
// CHECK-SAME:   i32 trunc (i64 sub (i64 ptrtoint ([1 x %swift.protocol_requirement]* [[B_REQTS:@".*"]] to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.protocol, %swift.protocol* @"$S17protocol_metadata1BMp", i32 0, i32 11) to i64)) to i32)
// CHECK: }

// CHECK: @"$S17protocol_metadata1CMp" = hidden constant %swift.protocol {
// -- flags: 1 = Swift | 4 = Needs Witness Table
// CHECK-SAME:   i32 72, i32 5,
// CHECK-SAME:   i32 1,
// CHECK-SAME:   i32 trunc (i64 sub (i64 ptrtoint ([1 x %swift.protocol_requirement]* [[C_REQTS:@".*"]] to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.protocol, %swift.protocol* @"$S17protocol_metadata1CMp", i32 0, i32 11) to i64)) to i32)
// CHECK-SAME: }

// -- @objc protocol O uses ObjC symbol mangling and layout
// CHECK: @_PROTOCOL__TtP17protocol_metadata1O_ = private constant { {{.*}} i32, [1 x i8*]*, i8*, i8* } {
// CHECK-SAME:   @_PROTOCOL_INSTANCE_METHODS__TtP17protocol_metadata1O_,
// -- size, flags: 1 = Swift
// CHECK-SAME:   i32 96, i32 1
// CHECK-SAME: @_PROTOCOL_METHOD_TYPES__TtP17protocol_metadata1O_
// CHECK-SAME: }

// CHECK: [[A_REQTS]] = internal constant [1 x %swift.protocol_requirement] [%swift.protocol_requirement { i32 17, i32 0, i32 0 }]
// CHECK: [[B_REQTS]] = internal constant [1 x %swift.protocol_requirement] [%swift.protocol_requirement { i32 17, i32 0, i32 0 }]
// CHECK: [[C_REQTS]] = internal constant [1 x %swift.protocol_requirement] [%swift.protocol_requirement { i32 17, i32 0, i32 0 }]

// -- @objc protocol OPT uses ObjC symbol mangling and layout
// CHECK: @_PROTOCOL__TtP17protocol_metadata3OPT_ = private constant { {{.*}} i32, [4 x i8*]*, i8*, i8* } {
// CHECK-SAME:   @_PROTOCOL_INSTANCE_METHODS_OPT__TtP17protocol_metadata3OPT_,
// CHECK-SAME:   @_PROTOCOL_CLASS_METHODS_OPT__TtP17protocol_metadata3OPT_,
// -- size, flags: 1 = Swift
// CHECK-SAME:   i32 96, i32 1
// CHECK-SAME: @_PROTOCOL_METHOD_TYPES__TtP17protocol_metadata3OPT_
// CHECK-SAME: }

// -- inheritance lists for refined protocols

// CHECK: [[AB_INHERITED:@.*]] = private constant { {{.*}}* } {
// CHECK:   i64 2,
// CHECK:   %swift.protocol* @"$S17protocol_metadata1AMp",
// CHECK:   %swift.protocol* @"$S17protocol_metadata1BMp"
// CHECK: }
// CHECK: [[AB_REQTS:@".*"]] = internal constant [3 x %swift.protocol_requirement] [%swift.protocol_requirement zeroinitializer, %swift.protocol_requirement zeroinitializer, %swift.protocol_requirement { i32 17, i32 0, i32 0 }]
// CHECK: @"$S17protocol_metadata2ABMp" = hidden constant %swift.protocol { 
// CHECK-SAME:   [[AB_INHERITED]]
// CHECK-SAME:   i32 72, i32 7,
// CHECK-SAME:   i32 3,
// CHECK-SAME:   i32 trunc (i64 sub (i64 ptrtoint ([3 x %swift.protocol_requirement]* [[AB_REQTS]] to i64), i64 ptrtoint (i32* getelementptr inbounds (%swift.protocol, %swift.protocol* @"$S17protocol_metadata2ABMp", i32 0, i32 11) to i64)) to i32)
// CHECK-SAME: }

// CHECK: [[ABO_INHERITED:@.*]] = private constant { {{.*}}* } {
// CHECK:   i64 3,
// CHECK:   %swift.protocol* @"$S17protocol_metadata1AMp",
// CHECK:   %swift.protocol* @"$S17protocol_metadata1BMp",
// CHECK:   {{.*}}* @_PROTOCOL__TtP17protocol_metadata1O_
// CHECK: }

protocol Comprehensive {
  associatedtype Assoc : A
  init()
  func instanceMethod()
  static func staticMethod()
  var instance: Assoc { get set }
  static var global: Assoc { get set }
}

// CHECK: [[COMPREHENSIVE_REQTS:@".*"]] = internal constant [11 x %swift.protocol_requirement]
// CHECK-SAME:  [%swift.protocol_requirement { i32 6, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 7, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 2, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 17, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 1, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 19, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 20, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 21, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 3, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 4, i32 0, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 5, i32 0, i32 0 }]

// CHECK: [[COMPREHENSIVE_ASSOC_NAME:@.*]] = private constant [6 x i8] c"Assoc\00"

// CHECK: @"$S17protocol_metadata13ComprehensiveMp" = hidden constant %swift.protocol
// CHECK-SAME: i32 72, i32 7, i32 11,
// CHECK-SAME: [11 x %swift.protocol_requirement]* [[COMPREHENSIVE_REQTS]]
// CHECK-SAME: i32 0
// CHECK-SAME: i32 trunc
// CHECK-SAME: [6 x i8]* [[COMPREHENSIVE_ASSOC_NAME]]

func reify_metadata<T>(_ x: T) {}

// CHECK: define hidden swiftcc void @"$S17protocol_metadata0A6_types{{[_0-9a-zA-Z]*}}F"
func protocol_types(_ a: A,
                    abc: A & B & C,
                    abco: A & B & C & O) {
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1AMp"
  // CHECK: call %swift.type* @swift_getExistentialTypeMetadata(i1 true, %swift.type* null, i64 1, %swift.protocol** {{%.*}})
  reify_metadata(a)
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1AMp"
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1BMp"
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1CMp"
  // CHECK: call %swift.type* @swift_getExistentialTypeMetadata(i1 false, %swift.type* null, i64 3, %swift.protocol** {{%.*}})
  reify_metadata(abc)
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1AMp"
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1BMp"
  // CHECK: store %swift.protocol* @"$S17protocol_metadata1CMp"
  // CHECK: [[O_REF:%.*]] = load i8*, i8** @"\01l_OBJC_PROTOCOL_REFERENCE_$__TtP17protocol_metadata1O_"
  // CHECK: [[O_REF_BITCAST:%.*]] = bitcast i8* [[O_REF]] to %swift.protocol*
  // CHECK: store %swift.protocol* [[O_REF_BITCAST]]
  // CHECK: call %swift.type* @swift_getExistentialTypeMetadata(i1 false, %swift.type* null, i64 4, %swift.protocol** {{%.*}})
  reify_metadata(abco)
}

