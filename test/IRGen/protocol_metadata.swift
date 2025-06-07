// RUN: %target-swift-frontend -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module -enable-objc-interop | %FileCheck %s -DINT=i%target-ptrsize

// REQUIRES: PTRSIZE=64
// UNSUPPORTED: CPU=arm64e

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

// CHECK: [[A_NAME:@.*]] = private constant [2 x i8] c"A\00"

// CHECK-LABEL: @"$s17protocol_metadata1AMp" = hidden constant
// CHECK-SAME:   i32 65603,
// CHECK-SAME:   @"$s17protocol_metadataMXM"
// CHECK-SAME:   [[A_NAME]]
// CHECK-SAME:   i32 0,
// CHECK-SAME:   i32 1,
// CHECK-SAME:   i32 0,
// CHECK-SAME: }

// CHECK: [[B_NAME:@.*]] = private constant [2 x i8] c"B\00"
// CHECK-LABEL: @"$s17protocol_metadata1BMp" = hidden constant
// CHECK-SAME:   i32 65603,
// CHECK-SAME:   @"$s17protocol_metadataMXM"
// CHECK-SAME:   i32 0,
// CHECK-SAME:   [[B_NAME]]
// CHECK-SAME:   i32 1,
// CHECK: }

// CHECK: [[C_NAME:@.*]] = private constant [2 x i8] c"C\00"
// CHECK-LABEL: @"$s17protocol_metadata1CMp" = hidden constant
// CHECK-SAME:   i32 67,
// CHECK-SAME:   @"$s17protocol_metadataMXM"
// CHECK-SAME:   [[C_NAME]]
// CHECK-SAME:   i32 1,
// CHECK-SAME:   i32 1,
// CHECK-SAME:   i32 0,
// AnyObject layout constraint
// CHECK-SAME:   i32 31,
// CHECK-SAME:   @"symbolic x"
// CHECK-SAME:   i32 0
// CHECK-SAME: }

// -- @objc protocol O uses ObjC symbol mangling and layout
// CHECK-LABEL: @_PROTOCOL__TtP17protocol_metadata1O_ = weak hidden constant
// CHECK-SAME:   @_PROTOCOL_INSTANCE_METHODS__TtP17protocol_metadata1O_,
// -- size, flags: 1 = Swift
// CHECK-SAME:   i32 96, i32 1
// CHECK-SAME: @_PROTOCOL_METHOD_TYPES__TtP17protocol_metadata1O_
// CHECK-SAME: }

// -- @objc protocol OPT uses ObjC symbol mangling and layout
// CHECK: @_PROTOCOL__TtP17protocol_metadata3OPT_ = weak hidden constant { {{.*}} i32, ptr, ptr, ptr } {
// CHECK-SAME:   @_PROTOCOL_INSTANCE_METHODS_OPT__TtP17protocol_metadata3OPT_,
// CHECK-SAME:   @_PROTOCOL_CLASS_METHODS_OPT__TtP17protocol_metadata3OPT_,
// -- size, flags: 1 = Swift
// CHECK-SAME:   i32 96, i32 1
// CHECK-SAME: @_PROTOCOL_METHOD_TYPES__TtP17protocol_metadata3OPT_
// CHECK-SAME: }

// -- inheritance lists for refined protocols

// CHECK: [[AB_NAME:@.*]] = private constant [3 x i8] c"AB\00"
// CHECK: @"$s17protocol_metadata2ABMp" = hidden constant
// CHECK-SAME:   i32 65603,
// CHECK-SAME:   @"$s17protocol_metadataMXM"
// CHECK-SAME:   [[AB_NAME]]
// CHECK-SAME:   i32 2, i32 3, i32 0

// Inheritance from A
// CHECK-SAME:   i32 128,
// CHECK-SAME:   @"symbolic x"
// CHECK-SAME: @"$s17protocol_metadata1AMp"

// Inheritance from B
// CHECK-SAME:   i32 128,
// CHECK-SAME:   @"symbolic x"
// CHECK-SAME:   @"$s17protocol_metadata1BMp"
// CHECK: }

protocol Comprehensive {
  associatedtype Assoc : A
  init()
  func instanceMethod()
  static func staticMethod()
  var instance: Assoc { get set }
  static var global: Assoc { get set }
}

// CHECK: [[COMPREHENSIVE_ASSOC_NAME:@.*]] = private constant [6 x i8] c"Assoc\00"

// CHECK: @"$s17protocol_metadata13ComprehensiveMp" = hidden constant
// CHECK-SAME: i32 65603
// CHECK-SAME: i32 1
// CHECK-SAME: i32 11,
// CHECK-SAME: i32 trunc
// CHECK-SAME: ptr [[COMPREHENSIVE_ASSOC_NAME]]
// CHECK-SAME:   %swift.protocol_requirement { i32 8, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 7, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 2, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 17, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 1, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 19, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 20, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 22, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 3, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 4, i32 0 },
// CHECK-SAME:   %swift.protocol_requirement { i32 6, i32 0 }

  struct ParentType {

    // NESTED: [[NESTED_NAME:@.*]] = private constant [7 x i8] c"Nested\00"
    // NESTED: [[NESTED_RETURNVALUE_NAME:@.*]] = private constant [12 x i8] c"ReturnValue\00"

    // NESTED: @"$s17protocol_metadata10ParentTypeV6NestedMp" = hidden constant
    // NESTED-SAME: i32 65603,
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeVMn"
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeV6NestedMp", i32 0, i32 1)
    // NESTED-SAME: [7 x i8]* [[NESTED_NAME]]
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeV6NestedMp", i32 0, i32 2)
    // NESTED-SAME: i32 0,
    // NESTED-SAME: i32 2,
    // NESTED-SAME: [12 x i8]* [[NESTED_RETURNVALUE_NAME]]
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeV6NestedMp", i32 0, i32 5)
    // NESTED-SAME: %swift.protocol_requirement { i32 7, i32 0 },
    // NESTED-SAME: %swift.protocol_requirement { i32 17, i32 0 }
    protocol Nested {
      associatedtype ReturnValue
      func doSomething() -> ReturnValue
    }
  }

  extension ParentType {

    // NESTED: [[NESTEDVIAEXT_NAME:@.*]] = private constant [19 x i8] c"NestedViaExtension\00"

    // NESTED: @"$s17protocol_metadata10ParentTypeV18NestedViaExtensionMp" = hidden constant
    // NESTED-SAME: i32 65603,
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeVMn"
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeV18NestedViaExtensionMp", i32 0, i32 1)
    // NESTED-SAME: [19 x i8]* [[NESTEDVIAEXT_NAME]]
    // NESTED-SAME: @"$s17protocol_metadata10ParentTypeV18NestedViaExtensionMp", i32 0, i32 2)
    // NESTED-SAME: i32 0,
    // NESTED-SAME: i32 1,
    // NESTED-SAME: i32 0,
    // NESTED-SAME: %swift.protocol_requirement { i32 17, i32 0 }
    protocol NestedViaExtension {
      func foo()
    }
  }

  func parentFunc() {
    // NESTED: @"$s17protocol_metadata10parentFuncyyF6NestedL_Mp" = internal constant
    // NESTED-SAME: i32 65603,
    // NESTED-SAME: @"$s17protocol_metadata10parentFuncyyF6NestedL_PMXX"
    // NESTED-SAME: @"$s17protocol_metadata10parentFuncyyF6NestedL_Mp", i32 0, i32 1)
    // NESTED-SAME: [7 x i8]* [[NESTED_NAME]]
    // NESTED-SAME: @"$s17protocol_metadata10parentFuncyyF6NestedL_Mp", i32 0, i32 2)
    // NESTED-SAME: i32 0,
    // NESTED-SAME: i32 1,
    // NESTED-SAME: i32 0,
    // NESTED-SAME: %swift.protocol_requirement { i32 17, i32 0 }
    protocol Nested {
      func foo()
    }
  }