// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/abi -F %clang-importer-sdk-path/frameworks %s -import-objc-header %S/Inputs/objc_implementation.h -emit-ir > %t.ir
// RUN: %FileCheck --input-file %t.ir %s
// RUN: %FileCheck --input-file %t.ir --check-prefix NEGATIVE %s
// REQUIRES: objc_interop

// CHECK: [[selector_data_init:@[^, ]+]] = private global [5 x i8] c"init\00", section "__TEXT,__objc_methname,cstring_literals", align 1
// CHECK: [[selector_data_mainMethod_:@[^, ]+]] = private global [12 x i8] c"mainMethod:\00", section "__TEXT,__objc_methname,cstring_literals", align 1

//
// @_objcImplementation class
//

// Metaclass
// CHECK: @"OBJC_METACLASS_$_ImplClass" = global %objc_class { %objc_class* @"OBJC_METACLASS_$_NSObject", %objc_class* @"OBJC_METACLASS_$_NSObject", %swift.opaque* @_objc_empty_cache, %swift.opaque* null, i64 ptrtoint ({ i32, i32, i32, i32, i8*, i8*, i8*, i8*, i8*, i8*, i8* }* [[_METACLASS_DATA_ImplClass:@[^, ]+]] to i64) }, align 8
// CHECK: [[OBJC_CLASS_NAME_:@[^, ]+]] = private unnamed_addr constant [10 x i8] c"ImplClass\00"
// CHECK: @_METACLASS_DATA_ImplClass = internal constant { i32, i32, i32, i32, i8*, i8*, i8*, i8*, i8*, i8*, i8* } { i32 129, i32 40, i32 40, i32 0, i8* null, i8* getelementptr inbounds ([10 x i8], [10 x i8]* [[OBJC_CLASS_NAME_:@[^, ]+]], i64 0, i64 0), i8* null, i8* null, i8* null, i8* null, i8* null }, section "__DATA, __objc_const", align 8
// TODO: Why the extra i32 field above?

// Class
// CHECK: [[selector_data_implProperty:@[^, ]+]] = private global [13 x i8] c"implProperty\00", section "__TEXT,__objc_methname,cstring_literals", align 1
// CHECK: [[OBJC_METH_VAR_TYPE_implProperty:@[^, ]+]] = private unnamed_addr constant [8 x i8] c"i16@0:8\00"
// CHECK: [[selector_data_setImplProperty_:@[^, ]+]] = private global [17 x i8] c"setImplProperty:\00", section "__TEXT,__objc_methname,cstring_literals", align 1
// CHECK: [[OBJC_METH_VAR_TYPE_mainMethod_:@[^, ]+]] = private unnamed_addr constant [11 x i8] c"v20@0:8i16\00"
// CHECK: [[_INSTANCE_METHODS_ImplClass:@[^, ]+]] = internal constant { i32, i32, [3 x { i8*, i8*, i8* }] } { i32 24, i32 3, [3 x { i8*, i8*, i8* }] [{ i8*, i8*, i8* } { i8* getelementptr inbounds ([13 x i8], [13 x i8]* [[selector_data_implProperty]], i64 0, i64 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[OBJC_METH_VAR_TYPE_implProperty]], i64 0, i64 0), i8* bitcast (i32 (%0*, i8*)* @"$sSo9ImplClassC19objc_implementationE12implPropertys5Int32VvgTo" to i8*) }, { i8*, i8*, i8* } { i8* getelementptr inbounds ([17 x i8], [17 x i8]* [[selector_data_setImplProperty_]], i64 0, i64 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[OBJC_METH_VAR_TYPE_mainMethod_]], i64 0, i64 0), i8* bitcast (void (%0*, i8*, i32)* @"$sSo9ImplClassC19objc_implementationE12implPropertys5Int32VvsTo" to i8*) }, { i8*, i8*, i8* } { i8* getelementptr inbounds ([12 x i8], [12 x i8]* [[selector_data_mainMethod_]], i64 0, i64 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[OBJC_METH_VAR_TYPE_mainMethod_]], i64 0, i64 0), i8* bitcast (void (%0*, i8*, i32)* @"$sSo9ImplClassC19objc_implementationE10mainMethodyys5Int32VFTo" to i8*) }] }, section "__DATA, __objc_data", align 8
// CHECK: [[OBJC_IVAR_NAME_implProperty:@[^, ]+]] = private unnamed_addr constant [13 x i8] c"implProperty\00"
// CHECK: [[OBJC_IVAR_TYPE_implProperty:@[^, ]+]] = private unnamed_addr constant [1 x i8] zeroinitializer
// CHECK: [[_IVARS_ImplClass:@[^, ]+]] = internal constant { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] } { i32 32, i32 1, [1 x { i64*, i8*, i8*, i32, i32 }] [{ i64*, i8*, i8*, i32, i32 } { i64* @"$sSo9ImplClassC19objc_implementationE12implPropertys5Int32VvpWvd", i8* getelementptr inbounds ([13 x i8], [13 x i8]* [[OBJC_IVAR_NAME_implProperty]], i64 0, i64 0), i8* getelementptr inbounds ([1 x i8], [1 x i8]* [[OBJC_IVAR_TYPE_implProperty]], i64 0, i64 0), i32 2, i32 4 }] }, section "__DATA, __objc_const", align 8
// CHECK: [[OBJC_PROPERTY_ATTRS_implProperty:@[^, ]+]] = private unnamed_addr constant [19 x i8] c"Ti,N,VimplProperty\00"
// CHECK: [[_PROPERTIES_ImplClass:@[^, ]+]] = internal constant { i32, i32, [1 x { i8*, i8* }] } { i32 16, i32 1, [1 x { i8*, i8* }] [{ i8*, i8* } { i8* getelementptr inbounds ([13 x i8], [13 x i8]* [[OBJC_IVAR_NAME_implProperty]], i64 0, i64 0), i8* getelementptr inbounds ([19 x i8], [19 x i8]* [[OBJC_PROPERTY_ATTRS_implProperty]], i64 0, i64 0) }] }, section "__DATA, __objc_const", align 8
// CHECK: [[_DATA_ImplClass:@[^, ]+]] = internal constant { i32, i32, i32, i32, i8*, i8*, { i32, i32, [3 x { i8*, i8*, i8* }] }*, i8*, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }*, i8*, { i32, i32, [1 x { i8*, i8* }] }* } { i32 128, i32 8, i32 12, i32 0, i8* null, i8* getelementptr inbounds ([10 x i8], [10 x i8]* [[OBJC_CLASS_NAME_]], i64 0, i64 0), { i32, i32, [3 x { i8*, i8*, i8* }] }* [[_INSTANCE_METHODS_ImplClass]], i8* null, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }* [[_IVARS_ImplClass]], i8* null, { i32, i32, [1 x { i8*, i8* }] }* [[_PROPERTIES_ImplClass]] }, section "__DATA, __objc_data", align 8
// CHECK: @"OBJC_CLASS_$_ImplClass" = global <{ i64, %objc_class*, %swift.opaque*, %swift.opaque*, { i32, i32, i32, i32, i8*, i8*, { i32, i32, [3 x { i8*, i8*, i8* }] }*, i8*, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }*, i8*, { i32, i32, [1 x { i8*, i8* }] }* }* }> <{ i64 ptrtoint (%objc_class* @"OBJC_METACLASS_$_ImplClass" to i64), %objc_class* @"OBJC_CLASS_$_NSObject", %swift.opaque* @_objc_empty_cache, %swift.opaque* null, { i32, i32, i32, i32, i8*, i8*, { i32, i32, [3 x { i8*, i8*, i8* }] }*, i8*, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }*, i8*, { i32, i32, [1 x { i8*, i8* }] }* }* [[_DATA_ImplClass]] }>, section "__DATA,__objc_data, regular", align 8

// Swift metadata
// NEGATIVE-NOT: OBJC_CLASS_$_ImplClass.{{[0-9]}}
// NEGATIVE-NOT: $sSo9ImplClassCMn
// NEGATIVE-NOT: $sSo9ImplClassCHn
// NEGATIVE-NOT: $sSo9ImplClassCN
// NEGATIVE-NOT: $sSo9ImplClassCMf
@_objcImplementation extension ImplClass {
  @objc var implProperty: Int32

  @objc func mainMethod(_: Int32) {
    print("mainMethod")
  }
}

//
// @_objcImplementation category
//

// Category1
// CHECK: [[OBJC_CLASS_NAME_2:@[^, ]+]] = private unnamed_addr constant [10 x i8] c"Category1\00"
// CHECK: [[selector_data_category1Method_:@[^, ]+]] = private global [17 x i8] c"category1Method:\00", section "__TEXT,__objc_methname,cstring_literals", align 1
// CHECK: [[_CATEGORY_INSTANCE_METHODS_ImplClass_Category1:@[^, ]+]] = internal constant { i32, i32, [1 x { i8*, i8*, i8* }] } { i32 24, i32 1, [1 x { i8*, i8*, i8* }] [{ i8*, i8*, i8* } { i8* getelementptr inbounds ([17 x i8], [17 x i8]* [[selector_data_category1Method_]], i64 0, i64 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[OBJC_METH_VAR_TYPE_mainMethod_]], i64 0, i64 0), i8* bitcast (void (%0*, i8*, i32)* @"$sSo9ImplClassC19objc_implementationE15category1Methodyys5Int32VFTo" to i8*) }] }, section "__DATA, __objc_data", align 8
// CHECK: [[_CATEGORY_ImplClass_Category1:@[^, ]+]] = internal constant { i8*, %objc_class*, { i32, i32, [1 x { i8*, i8*, i8* }] }*, i8*, i8*, i8*, i8*, i32 } { i8* getelementptr inbounds ([10 x i8], [10 x i8]* [[OBJC_CLASS_NAME_2]], i64 0, i64 0), %objc_class* bitcast (<{ i64, %objc_class*, %swift.opaque*, %swift.opaque*, { i32, i32, i32, i32, i8*, i8*, { i32, i32, [3 x { i8*, i8*, i8* }] }*, i8*, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }*, i8*, { i32, i32, [1 x { i8*, i8* }] }* }* }>* @"OBJC_CLASS_$_ImplClass" to %objc_class*), { i32, i32, [1 x { i8*, i8*, i8* }] }* [[_CATEGORY_INSTANCE_METHODS_ImplClass_Category1]], i8* null, i8* null, i8* null, i8* null, i32 60 }, section "__DATA, __objc_const", align 8
@_objcImplementation(Category1) extension ImplClass {
  @objc func category1Method(_: Int32) {
    print("category1Method")
  }
}

//
// @objc subclass of @_objcImplementation class
//

// Metaclass
// CHECK: @"OBJC_METACLASS_$__TtC19objc_implementation13SwiftSubclass" = global %objc_class { %objc_class* @"OBJC_METACLASS_$_NSObject", %objc_class* @"OBJC_METACLASS_$_ImplClass", %swift.opaque* @_objc_empty_cache, %swift.opaque* null, i64 ptrtoint ({ i32, i32, i32, i32, i8*, i8*, i8*, i8*, i8*, i8*, i8* }* [[_METACLASS_DATA_SwiftSubclass:@[^, ]+]] to i64) }, align 8
// CHECK: [[OBJC_CLASS_NAME_3:@[^, ]+]] = private unnamed_addr constant [41 x i8] c"_TtC19objc_implementation13SwiftSubclass\00"
// CHECK: [[_METACLASS_DATA_SwiftSubclass]] = internal constant { i32, i32, i32, i32, i8*, i8*, i8*, i8*, i8*, i8*, i8* } { i32 129, i32 40, i32 40, i32 0, i8* null, i8* getelementptr inbounds ([41 x i8], [41 x i8]* [[OBJC_CLASS_NAME_3]], i64 0, i64 0), i8* null, i8* null, i8* null, i8* null, i8* null }, section "__DATA, __objc_const", align 8

// Class
// CHECK: [[OBJC_METH_VAR_TYPE_2:@[^, ]+]] = private unnamed_addr constant [8 x i8] c"@16@0:8\00"
// CHECK: [[_INSTANCE_METHODS_SwiftSubclass:@[^, ]+]] = internal constant { i32, i32, [2 x { i8*, i8*, i8* }] } { i32 24, i32 2, [2 x { i8*, i8*, i8* }] [{ i8*, i8*, i8* } { i8* getelementptr inbounds ([12 x i8], [12 x i8]* [[selector_data_mainMethod_]], i64 0, i64 0), i8* getelementptr inbounds ([11 x i8], [11 x i8]* [[OBJC_METH_VAR_TYPE_mainMethod_]], i64 0, i64 0), i8* bitcast (void (%1*, i8*, i32)* @"$s19objc_implementation13SwiftSubclassC10mainMethodyys5Int32VFTo" to i8*) }, { i8*, i8*, i8* } { i8* getelementptr inbounds ([5 x i8], [5 x i8]* [[selector_data_init]], i64 0, i64 0), i8* getelementptr inbounds ([8 x i8], [8 x i8]* [[OBJC_METH_VAR_TYPE_2]], i64 0, i64 0), i8* bitcast (%1* (%1*, i8*)* @"$s19objc_implementation13SwiftSubclassCACycfcTo" to i8*) }] }, section "__DATA, __objc_data", align 8
// CHECK: [[_DATA_SwiftSubclass:@[^, ]+]] = internal constant { i32, i32, i32, i32, i8*, i8*, { i32, i32, [2 x { i8*, i8*, i8* }] }*, i8*, i8*, i8*, i8* } { i32 128, i32 12, i32 12, i32 0, i8* null, i8* getelementptr inbounds ([41 x i8], [41 x i8]* [[OBJC_CLASS_NAME_3]], i64 0, i64 0), { i32, i32, [2 x { i8*, i8*, i8* }] }* [[_INSTANCE_METHODS_SwiftSubclass]], i8* null, i8* null, i8* null, i8* null }, section "__DATA, __objc_data", align 8

// Swift metadata
// CHECK: [[module_name:@[^, ]+]] = private constant [20 x i8] c"objc_implementation\00"
// CHECK: @"$s19objc_implementationMXM" = linkonce_odr hidden constant <{ i32, i32, i32 }> <{ i32 0, i32 0, i32 trunc (i64 sub (i64 ptrtoint ([20 x i8]* [[module_name]] to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i32, i32, i32 }>, <{ i32, i32, i32 }>* @"$s19objc_implementationMXM", i32 0, i32 2) to i64)) to i32) }>, section "__TEXT,__const", align 4
// CHECK: [[READABLE_SWIFT_CLASS_NAME_SwiftSubclass:@[^, ]+]] = private constant [14 x i8] c"SwiftSubclass\00"
// CHECK: @"symbolic So9ImplClassC" = linkonce_odr hidden constant <{ [13 x i8], i8 }> <{ [13 x i8] c"So9ImplClassC", i8 0 }>, section "__TEXT,__swift5_typeref, regular", align 2
// CHECK: @"$s19objc_implementation13SwiftSubclassCMn" = constant <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }> <{ i32 80, i32 trunc (i64 sub (i64 ptrtoint (<{ i32, i32, i32 }>* @"$s19objc_implementationMXM" to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn", i32 0, i32 1) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint ([14 x i8]* [[READABLE_SWIFT_CLASS_NAME_SwiftSubclass]] to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn", i32 0, i32 2) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (%swift.metadata_response (i64)* @"$s19objc_implementation13SwiftSubclassCMa" to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn", i32 0, i32 3) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint ({ i32, i32, i16, i16, i32 }* @"$s19objc_implementation13SwiftSubclassCMF" to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn", i32 0, i32 4) to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [13 x i8], i8 }>* @"symbolic So9ImplClassC" to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn", i32 0, i32 5) to i64)) to i32), i32 2, i32 10, i32 0, i32 0, i32 10 }>, section "__TEXT,__const", align 4
// CHECK: @"$s19objc_implementation13SwiftSubclassCMf" = internal global <{ void (%T19objc_implementation13SwiftSubclassC*)*, i8**, i64, %objc_class*, %swift.opaque*, %swift.opaque*, i64, i32, i32, i32, i16, i16, i32, i32, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>*, i8* }> <{ void (%T19objc_implementation13SwiftSubclassC*)* @"$s19objc_implementation13SwiftSubclassCfD", i8** @"$sBOWV", i64 ptrtoint (%objc_class* @"OBJC_METACLASS_$__TtC19objc_implementation13SwiftSubclass" to i64), %objc_class* bitcast (<{ i64, %objc_class*, %swift.opaque*, %swift.opaque*, { i32, i32, i32, i32, i8*, i8*, { i32, i32, [3 x { i8*, i8*, i8* }] }*, i8*, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }*, i8*, { i32, i32, [1 x { i8*, i8* }] }* }* }>* @"OBJC_CLASS_$_ImplClass" to %objc_class*), %swift.opaque* @_objc_empty_cache, %swift.opaque* null, i64 add (i64 ptrtoint ({ i32, i32, i32, i32, i8*, i8*, { i32, i32, [2 x { i8*, i8*, i8* }] }*, i8*, i8*, i8*, i8* }* @_DATA__TtC19objc_implementation13SwiftSubclass to i64), i64 1), i32 0, i32 0, i32 12, i16 7, i16 0, i32 96, i32 16, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn", i8* null }>, section "__DATA,__objc_data, regular", align 8
// CHECK: @"symbolic _____ 19objc_implementation13SwiftSubclassC" = linkonce_odr hidden constant <{ i8, i32, i8 }> <{ i8 1, i32 trunc (i64 sub (i64 ptrtoint (<{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>* @"$s19objc_implementation13SwiftSubclassCMn" to i64), i64 ptrtoint (i32* getelementptr inbounds (<{ i8, i32, i8 }>, <{ i8, i32, i8 }>* @"symbolic _____ 19objc_implementation13SwiftSubclassC", i32 0, i32 1) to i64)) to i32), i8 0 }>, section "__TEXT,__swift5_typeref, regular", align 2
// CHECK: @"$s19objc_implementation13SwiftSubclassCMF" = internal constant { i32, i32, i16, i16, i32 } { i32 trunc (i64 sub (i64 ptrtoint (<{ i8, i32, i8 }>* @"symbolic _____ 19objc_implementation13SwiftSubclassC" to i64), i64 ptrtoint ({ i32, i32, i16, i16, i32 }* @"$s19objc_implementation13SwiftSubclassCMF" to i64)) to i32), i32 trunc (i64 sub (i64 ptrtoint (<{ [13 x i8], i8 }>* @"symbolic So9ImplClassC" to i64), i64 ptrtoint (i32* getelementptr inbounds ({ i32, i32, i16, i16, i32 }, { i32, i32, i16, i16, i32 }* @"$s19objc_implementation13SwiftSubclassCMF", i32 0, i32 1) to i64)) to i32), i16 7, i16 12, i32 0 }, section "__TEXT,__swift5_fieldmd, regular", align 4
open class SwiftSubclass: ImplClass {
  override open func mainMethod(_: Int32) {
    print("subclass mainMethod")
  }
}

//
// Epilogue
//

// CHECK: @"objc_classes_OBJC_CLASS_$_ImplClass" = internal global i8* bitcast (<{ i64, %objc_class*, %swift.opaque*, %swift.opaque*, { i32, i32, i32, i32, i8*, i8*, { i32, i32, [3 x { i8*, i8*, i8* }] }*, i8*, { i32, i32, [1 x { i64*, i8*, i8*, i32, i32 }] }*, i8*, { i32, i32, [1 x { i8*, i8* }] }* }* }>* @"OBJC_CLASS_$_ImplClass" to i8*), section "__DATA,__objc_classlist,regular,no_dead_strip", align 8
// CHECK: @"objc_classes_$s19objc_implementation13SwiftSubclassCN" = internal global i8* bitcast (%swift.type* @"$s19objc_implementation13SwiftSubclassCN" to i8*), section "__DATA,__objc_classlist,regular,no_dead_strip", align 8
// CHECK: @objc_categories = internal global [1 x i8*] [i8* bitcast ({ i8*, %objc_class*, { i32, i32, [1 x { i8*, i8*, i8* }] }*, i8*, i8*, i8*, i8*, i32 }* [[_CATEGORY_ImplClass_Category1]] to i8*)], section "__DATA,__objc_catlist,regular,no_dead_strip", align 8

// CHECK: @"$s19objc_implementation13SwiftSubclassCN" = alias %swift.type, bitcast (i64* getelementptr inbounds (<{ void (%T19objc_implementation13SwiftSubclassC*)*, i8**, i64, %objc_class*, %swift.opaque*, %swift.opaque*, i64, i32, i32, i32, i16, i16, i32, i32, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>*, i8* }>, <{ void (%T19objc_implementation13SwiftSubclassC*)*, i8**, i64, %objc_class*, %swift.opaque*, %swift.opaque*, i64, i32, i32, i32, i16, i16, i32, i32, <{ i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32 }>*, i8* }>* @"$s19objc_implementation13SwiftSubclassCMf", i32 0, i32 2) to %swift.type*)
// CHECK: @"OBJC_CLASS_$__TtC19objc_implementation13SwiftSubclass" = alias %swift.type, %swift.type* @"$s19objc_implementation13SwiftSubclassCN"

//
// Functions
//

public func fn(impl: ImplClass, swiftSub: SwiftSubclass) {
  impl.mainMethod(0)
  swiftSub.mainMethod(1)
}

// CHECK: define internal void @"$sSo9ImplClassC19objc_implementationE10mainMethodyys5Int32VFTo"
// CHECK: define internal void @"$sSo9ImplClassC19objc_implementationE15category1Methodyys5Int32VFTo"
// CHECK: define internal void @"$s19objc_implementation13SwiftSubclassC10mainMethodyys5Int32VFTo"
// CHECK: define internal %1* @"$s19objc_implementation13SwiftSubclassCACycfcTo"

// CHECK-LABEL: define swiftcc void @"$s19objc_implementation2fn4impl8swiftSubySo9ImplClassC_AA13SwiftSubclassCtF"
// CHECK:   [[SEL_1:%[^ ]+]] = load i8*, i8** @"\01L_selector(mainMethod:)", align 8
// CHECK:   [[PARAM_impl:%[^ ]+]] = bitcast %TSo9ImplClassC* %0 to %0*
// CHECK:   call void bitcast (void ()* @objc_msgSend to void (%0*, i8*, i32)*)(%0* [[PARAM_impl]], i8* [[SEL_1]], i32 0)
// CHECK:   [[SEL_2:%[^ ]+]] = load i8*, i8** @"\01L_selector(mainMethod:)", align 8
// CHECK:   [[PARAM_swiftSub:%[^ ]+]] = bitcast %T19objc_implementation13SwiftSubclassC* %1 to %1*
// CHECK:   call void bitcast (void ()* @objc_msgSend to void (%1*, i8*, i32)*)(%1* [[PARAM_swiftSub]], i8* [[SEL_2]], i32 1)
// CHECK:   ret void
// CHECK: }

//
// Not implemented in Swift
// 

// NEGATIVE-NOT: Category2
// NEGATIVE-NOT: NoImplClass
