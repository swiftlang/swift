// RUN: rm -rf %t && mkdir %t
// RUN: %build-irgen-test-overlays
// RUN: %swift -sdk %S/Inputs -I %t -primary-file %s -emit-ir | FileCheck %s

// CHECK: [[SGIZMO:C13objc_subclass10SwiftGizmo]] = type
// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[INT:%Si]] = type <{ i64 }>
// CHECK: [[OBJC_CLASS:%objc_class]] = type
// CHECK: [[OPAQUE:%swift.opaque]] = type
// CHECK: [[GIZMO:%CSo5Gizmo]] = type opaque
// CHECK: [[OBJC:%objc_object]] = type opaque

// CHECK: @_TWvdvC13objc_subclass10SwiftGizmo1xSi = global i64 16, align 8
// CHECK: @"OBJC_METACLASS_$__TtC13objc_subclass10SwiftGizmo" = global [[OBJC_CLASS]] { [[OBJC_CLASS]]* @"OBJC_METACLASS_$_NSObject", [[OBJC_CLASS]]* @"OBJC_METACLASS_$_Gizmo", [[OPAQUE]]* @_objc_empty_cache, [[OPAQUE]]* @_objc_empty_vtable, i64 ptrtoint ({{.*}} @_METACLASS_DATA__TtC13objc_subclass10SwiftGizmo to i64) }
// CHECK: [[STRING_X:@.*]] = private unnamed_addr constant [2 x i8] c"x\00"
// CHECK: [[STRING_EMPTY:@.*]] = private unnamed_addr constant [1 x i8] zeroinitializer
// CHECK: [[METHOD_TYPE_ENCODING1:@.*]] = private unnamed_addr constant [8 x i8] c"q16@0:8\00"
// CHECK: [[METHOD_TYPE_ENCODING2:@.*]] = private unnamed_addr constant [11 x i8] c"v24@0:8q16\00"
// CHECK: [[GETTER_ENCODING:@.*]] = private unnamed_addr constant [8 x i8] c"@16@0:8\00"
// CHECK: [[INIT_ENCODING:@.*]] = private unnamed_addr constant [14 x i8] c"@32@0:8q16@24\00"
// CHECK: [[DEALLOC_ENCODING:@.*]] = private unnamed_addr constant [11 x i8] c"v24@0:8@16\00"
// CHECK: [[STRING_SWIFTGIZMO:@.*]] = private unnamed_addr constant [32 x i8] c"_TtC13objc_subclass10SwiftGizmo\00"

// CHECK: @_METACLASS_DATA__TtC13objc_subclass10SwiftGizmo = private constant { {{.*}}* } {
// CHECK:   i32 129,
// CHECK:   i32 40,
// CHECK:   i32 40,
// CHECK:   i32 0,
// CHECK:   i8* null,
// CHECK:   i8* getelementptr inbounds ([{{[0-9]+}} x i8]* [[STRING_SWIFTGIZMO]], i64 0, i64 0),
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* null
// CHECK: }, section "__DATA, __objc_const", align 8

// CHECK: @_INSTANCE_METHODS__TtC13objc_subclass10SwiftGizmo = private constant { {{.*}}] } {
// CHECK:   i32 24,
// CHECK:   i32 11,
// CHECK:   [11 x { i8*, i8*, i8* }] [{
// CHECK:      i8* getelementptr inbounds ([2 x i8]* @"\01L_selector_data(x)", i64 0, i64 0),
// CHECK:      i8* getelementptr inbounds ([8 x i8]* [[METHOD_TYPE_ENCODING1]], i64 0, i64 0)
// CHECK:      i8* bitcast (i64 ([[OPAQUE2:%.*]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmog1xSi to i8*)
// CHECK:   }, {
// CHECK:      i8* getelementptr inbounds ([6 x i8]* @"\01L_selector_data(setX:)", i64 0, i64 0),
// CHECK:      i8* getelementptr inbounds ([11 x i8]* [[METHOD_TYPE_ENCODING2]], i64 0, i64 0)
// CHECK:      i8* bitcast (void ([[OPAQUE3:%.*]]*, i8*, i64)* @_TToFC13objc_subclass10SwiftGizmos1xSi to i8*)
// CHECK:   }, {
// CHECK:      i8* getelementptr inbounds ([5 x i8]* @"\01L_selector_data(getX)", i64 0, i64 0),
// CHECK:      i8* getelementptr inbounds ([8 x i8]* [[METHOD_TYPE_ENCODING1]], i64 0, i64 0)
// CHECK:      i8* bitcast (i64 ([[OPAQUE2]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmo4getXfS0_FT_Si to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([10 x i8]* @"\01L_selector_data(duplicate)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([8 x i8]* [[GETTER_ENCODING]], i64 0, i64 0),
// CHECK:     i8* bitcast ([[OPAQUE1:.*]]* ([[OPAQUE0:%.*]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmo9duplicatefS0_FT_CSo5Gizmo to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), 
// CHECK:     i8* getelementptr inbounds ([8 x i8]* [[GETTER_ENCODING]], i64 0, i64 0),
// CHECK:     i8* bitcast ([[OPAQUE5:.*]]* ([[OPAQUE6:.*]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmocfMS0_FT_S0_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([20 x i8]* @"\01L_selector_data(initWithInt:string:)", i64 0, i64 0), 
// CHECK:     i8* getelementptr inbounds ([14 x i8]* [[INIT_ENCODING]], i64 0, i64 0),
// CHECK:     i8* bitcast ([[OPAQUE7:%.*]]* ([[OPAQUE8:%.*]]*, i8*, i64, [[OPAQUEONE:.*]]*)* @_TToFC13objc_subclass10SwiftGizmocfMS0_FT3intSi6stringSS_S0_ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([8 x i8]* @"\01L_selector_data(dealloc)", i64 0, i64 0), 
// CHECK:     i8* getelementptr inbounds ([11 x i8]* {{@[0-9]+}}, i64 0, i64 0),
// CHECK:     i8* bitcast (void ([[OPAQUE10:%.*]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmoD to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([10 x i8]* @"\01L_selector_data(isEnabled)", i64 0, i64 0), 
// CHECK:     i8* getelementptr inbounds ([8 x i8]* {{@[0-9]+}}, i64 0, i64 0), 
// CHECK:     i8* bitcast (i8 ([[OPAQUE11:%.*]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmog7enabledSb to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(setIsEnabled:)", i64 0, i64 0), 
// CHECK:     i8* getelementptr inbounds ([11 x i8]* {{@[0-9]+}}, i64 0, i64 0), 
// CHECK:     i8* bitcast (void ([[OPAQUE12:%.*]]*, i8*, i8)* @_TToFC13objc_subclass10SwiftGizmos7enabledSb to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([17 x i8]* @"\01L_selector_data(initWithBellsOn:)", i64 0, i64 0), 
// CHECK:     i8* getelementptr inbounds ([11 x i8]* {{@[0-9]+}}, i64 0, i64 0), 
// CHECK:     i8* bitcast ([[OPAQUE11:%.*]]* ([[OPAQUE12:%.*]]*, i8*, i64)* @_TToFC13objc_subclass10SwiftGizmocfMS0_FT7bellsOnSi_GSQS0__ to i8*)
// CHECK:   }, {
// CHECK:     i8* getelementptr inbounds ([15 x i8]* @"\01L_selector_data(.cxx_construct)", i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([3 x i8]* {{@[0-9]+}}, i64 0, i64 0),
// CHECK:     i8* bitcast ([[OPAQUE5]]* ([[OPAQUE6]]*, i8*)* @_TToFC13objc_subclass10SwiftGizmoe to i8*)
// CHECK:   }]
// CHECK: }, section "__DATA, __objc_const", align 8
// CHECK: @_IVARS__TtC13objc_subclass10SwiftGizmo = private constant { {{.*}}] } {
// CHECK:   i32 32,
// CHECK:   i32 1,
// CHECK:   [1 x { i64*, i8*, i8*, i32, i32 }] [{ i64*, i8*, i8*, i32, i32 } {
// CHECK:     i64* @_TWvdvC13objc_subclass10SwiftGizmo1xSi,
// CHECK:     i8* getelementptr inbounds ([2 x i8]* [[STRING_X]], i64 0, i64 0),
// CHECK:     i8* getelementptr inbounds ([1 x i8]* [[STRING_EMPTY]], i64 0, i64 0),
// CHECK:     i32 3,
// CHECK:     i32 8 }]
// CHECK: }, section "__DATA, __objc_const", align 8
// CHECK: @_DATA__TtC13objc_subclass10SwiftGizmo = private constant { {{.*}}* } {
// CHECK:    i32 132,
// CHECK:    i32 16,
// CHECK:    i32 24,
// CHECK:    i32 0,
// CHECK:    i8* null,
// CHECK:    i8* getelementptr inbounds ([{{[0-9]+}} x i8]* [[STRING_SWIFTGIZMO]], i64 0, i64 0),
// CHECK:    @_INSTANCE_METHODS__TtC13objc_subclass10SwiftGizmo,
// CHECK:    i8* null,
// CHECK:    @_IVARS__TtC13objc_subclass10SwiftGizmo,
// CHECK:    i8* null,
// CHECK:  }, section "__DATA, __objc_const", align 8
// CHECK-NOT: @_TMdCSo13SwiftGizmo = {{.*NSObject}}

// CHECK: @_INSTANCE_METHODS__TtC13objc_subclass12GenericGizmo

// CHECK: @_INSTANCE_METHODS__TtC13objc_subclass11SwiftGizmo2 = private constant { i32, {{.*}}] } { 
// CHECK:   i32 24, 
// CHECK:   i32 5, 
// CHECK:   [5 x { i8*, i8*, i8* }] [
// CHECK:     { 
// CHECK:       i8* getelementptr inbounds ([3 x i8]* @"\01L_selector_data(sg)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_ENCODING]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE13:%.*]]* ([[OPAQUE14:%.*]]*, i8*)* @_TToFC13objc_subclass11SwiftGizmo2g2sgCS_10SwiftGizmo to i8*)
// CHECK:     }, { 
// CHECK:       i8* getelementptr inbounds ([7 x i8]* @"\01L_selector_data(setSg:)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([11 x i8]* [[DEALLOC_ENCODING]], i64 0, i64 0),
// CHECK:       i8* bitcast (void ([[OPAQUE15:%.*]]*, i8*, [[OPAQUE16:%.*]]*)* @_TToFC13objc_subclass11SwiftGizmo2s2sgCS_10SwiftGizmo to i8*)
// CHECK:     }, { 
// CHECK:       i8* getelementptr inbounds ([5 x i8]* @"\01L_selector_data(init)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([8 x i8]* [[GETTER_ENCODING]], i64 0, i64 0),
// CHECK:       i8* bitcast ([[OPAQUE18:%.*]]* ([[OPAQUE19:%.*]]*, i8*)* @_TToFC13objc_subclass11SwiftGizmo2cfMS0_FT_S0_ to i8*)
// CHECK:     }, {
// CHECK:       i8* getelementptr inbounds ([17 x i8]* @"\01L_selector_data(initWithBellsOn:)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([11 x i8]* {{@[0-9]+}}, i64 0, i64 0), 
// CHECK:       i8* bitcast ([[OPAQUE21:%.*]]* ([[OPAQUE22:%.*]]*, i8*, i64)* @_TToFC13objc_subclass11SwiftGizmo2cfMS0_FT7bellsOnSi_GSQS0__ to i8*)
// CHECK:     }, { 
// CHECK:       i8* getelementptr inbounds ([14 x i8]* @"\01L_selector_data(.cxx_destruct)", i64 0, i64 0), 
// CHECK:       i8* getelementptr inbounds ([3 x i8]* {{@[0-9]+}}, i64 0, i64 0)
// CHECK:       i8* bitcast (void ([[OPAQUE20:%.*]]*, i8*)* @_TToFC13objc_subclass11SwiftGizmo2E to i8*)
// CHECK:     }
// CHECK: ] }

// CHECK: @objc_classes = internal global [2 x i8*] [i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_heapmetadata* bitcast ({{.*}}* @_TMdC13objc_subclass10SwiftGizmo to %swift.full_heapmetadata*), i32 0, i32 2) to i8*), i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_heapmetadata* bitcast ({{.*}}* @_TMdC13objc_subclass11SwiftGizmo2 to %swift.full_heapmetadata*), i32 0, i32 2) to i8*)], section "__DATA, __objc_classlist, regular, no_dead_strip", align 8

// CHECK: @objc_non_lazy_classes = internal global [1 x i8*] [i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_heapmetadata* bitcast ({{.*}}* @_TMdC13objc_subclass11SwiftGizmo2 to %swift.full_heapmetadata*), i32 0, i32 2) to i8*)], section "__DATA, __objc_nlclslist, regular, no_dead_strip", align 8

import gizmo

@requires_stored_property_inits
class SwiftGizmo : Gizmo {
  var x = Int()

  func getX() -> Int {
    return x;
  }

  override func duplicate() -> Gizmo {
    return SwiftGizmo()
  }

  override init() {
    super.init(bellsOn:0)
  }

  init(int i: Int, string str : String) {
    super.init(bellsOn:i)
  }

  deinit { var x = 10 }

  var enabled: Bool {
    @objc(isEnabled) get {
      return true
    }

    @objc(setIsEnabled:) set {
    }  
  }
}

class GenericGizmo<T> : Gizmo {
  func foo() {}

  var x : Int {
    return 0
  }

  var array : [T] = []
}
// CHECK: define hidden i64 @_TFC13objc_subclass12GenericGizmog1xSi(

var sg = SwiftGizmo()
sg.duplicate()

@objc_non_lazy_realization
class SwiftGizmo2 : Gizmo {
  var sg : SwiftGizmo

  override init() { 
    sg = SwiftGizmo()
    super.init()
  }

  deinit { }
}

