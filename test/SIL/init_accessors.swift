// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature InitAccessors %s | %FileCheck %s

struct TestInit {
  var x: Int
  var y: Int
  var full: (Int, Int)

  var point: (Int, Int) {
    // CHECK-LABEL: sil private @$s14init_accessors8TestInitV5pointSi_Sitvi : $@convention(thin) (Int, Int, @inout Int) -> (@out Int, @out (Int, Int))
    // CHECK: bb0([[Y_REF:%.*]] : $*Int, [[FULL_REF:%.*]] : $*(Int, Int), [[X_VAL:%.*]] : $Int, [[Y_VAL:%.*]] : $Int, [[X_REF:%.*]] : $*Int):
    //
    // CHECK: [[Y_ACCESS:%.*]] = begin_access [modify] [static] [[Y_REF]] : $*Int
    // CHECK-NEXT: store [[Y_VAL]] to [[Y_ACCESS]] : $*Int
    // CHECK-NEXT: end_access [[Y_ACCESS]] : $*Int
    //
    // CHECK-NEXT: [[X_ACCESS:%.*]] = begin_access [read] [static] [[X_REF]] : $*Int
    // CHECK-NEXT: [[X_VAL:%.*]] = load [[X_ACCESS]] : $*Int
    // CHECK-NEXT: end_access [[X_ACCESS]] : $*Int
    //
    // CHECK-NEXT: [[Y_ACCESS:%.*]] = begin_access [read] [static] [[Y_REF]] : $*Int
    // CHECK-NEXT: [[Y_VAL:%.*]] = load [[Y_ACCESS]] : $*Int
    // CHECK-NEXT: end_access [[Y_ACCESS]] : $*Int
    //
    // CHECK-NEXT: [[FULL_ACCESS:%.*]] = begin_access [modify] [static] [[FULL_REF]] : $*(Int, Int)
    // CHECK-NEXT: [[FULL_ELT_0:%.*]] = tuple_element_addr [[FULL_ACCESS]] : $*(Int, Int), 0
    // CHECK-NEXT: store [[X_VAL]] to [[FULL_ELT_0]] : $*Int
    // CHECK-NEXT: [[FULL_ELT_1:%.*]] = tuple_element_addr [[FULL_ACCESS]] : $*(Int, Int), 1
    // CHECK-NEXT: store [[Y_VAL]] to [[FULL_ELT_1]] : $*Int
    // CHECK-NEXT: end_access [[FULL_ACCESS]] : $*(Int, Int)
    init(initialValue) initializes(y full) accesses(x) {
      self.y = initialValue.1
      self.full = (self.x, self.y)
    }

    get { full }
    set { full = newValue }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors8TestInitV1x1yACSi_SitcfC : $@convention(method) (Int, Int, @thin TestInit.Type) -> TestInit
  // CHECK: // function_ref TestInit.point.init
  // CHECK-NEXT: [[INIT_ACCESSOR:%.*]] = function_ref @$s14init_accessors8TestInitV5pointSi_Sitvi : $@convention(thin) (Int, Int, @inout Int) -> (@out Int, @out (Int, Int))
  // CHECK: [[SELF_VALUE:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*TestInit
  // CHECK: [[Y_REF:%.*]] = struct_element_addr [[SELF_VALUE]] : $*TestInit, #TestInit.y
  // CHECK-NEXT: [[FULL_REF:%.*]] = struct_element_addr [[SELF_VALUE]] : $*TestInit, #TestInit.full
  // CHECK-NEXT: [[X_REF:%.*]] = struct_element_addr [[SELF_VALUE]] : $*TestInit, #TestInit.x
  // CHECK-NEXT: {{.*}} = apply [[INIT_ACCESSOR]]([[Y_REF]], [[FULL_REF]], %0, %1, [[X_REF]]) : $@convention(thin) (Int, Int, @inout Int) -> (@out Int, @out (Int, Int))
  // CHECK-NEXT: end_access [[SELF_VALUE]] : $*TestInit
  init(x: Int, y: Int) {
    self.x = x
    self.point = (x, y)
  }
}

struct TestSetter {
  var x: Int
  var y: Int

  var point: (Int, Int) {
    init(initialValue) accesses(x y) {
    }

    get { (x, y) }
    set { }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors10TestSetterV1x1yACSi_SitcfC : $@convention(method) (Int, Int, @thin TestSetter.Type) -> TestSetter
  // CHECK: [[SETTER_REF:%.*]] = function_ref @$s14init_accessors10TestSetterV5pointSi_Sitvs : $@convention(method) (Int, Int, @inout TestSetter) -> ()
  // CHECK-NEXT: [[SETTER_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[SETTER_REF]]([[SELF_VALUE:%.*]]) : $@convention(method) (Int, Int, @inout TestSetter) -> ()
  // CHECk-NEXT: %18 = apply [[SETTER_CLOSURE]](%0, %1) : $@callee_guaranteed (Int, Int) -> ()
  init(x: Int, y: Int) {
    self.x = x
    self.y = y
    self.point = (x, y)
  }
}

struct TestInitThenSetter {
  var x: Int
  var y: Int

  var point: (Int, Int) {
    init(initialValue) initializes(x y) {
      self.x = initialValue.0
      self.y = initialValue.1
    }

    get { (x, y) }
    set { }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors18TestInitThenSetterV1x1yACSi_SitcfC : $@convention(method) (Int, Int, @thin TestInitThenSetter.Type) -> TestInitThenSetter
  // CHECK: [[INIT_ACCESSOR:%.*]] = function_ref @$s14init_accessors18TestInitThenSetterV5pointSi_Sitvi : $@convention(thin) (Int, Int) -> (@out Int, @out Int)
  // CHECK: [[X_REF:%.*]] = struct_element_addr {{.*}} : $*TestInitThenSetter, #TestInitThenSetter.x
  // CHECK-NEXT: [[Y_REF:%.*]] = struct_element_addr {{.*}} : $*TestInitThenSetter, #TestInitThenSetter.y
  // CHECK-NEXT: {{.*}} = apply [[INIT_ACCESSOR]]([[X_REF]], [[Y_REF]], %0, %1) : $@convention(thin) (Int, Int) -> (@out Int, @out Int)
  //
  // CHECK: [[BUILTIN_ZERO:%.*]] = integer_literal $Builtin.Int64, 0
  // CHECK-NEXT: [[ZERO_X:%.*]] = struct $Int ([[BUILTIN_ZERO]] : $Builtin.Int64)
  // CHECK-NEXT: [[BUILTIN_ZERO:%.*]] = integer_literal $Builtin.Int64, 0
  // CHECK-NEXT: [[ZERO_Y:%.*]] = struct $Int ([[BUILTIN_ZERO]] : $Builtin.Int64)
  // CHECK: [[SETTER_REF:%.*]] = function_ref @$s14init_accessors18TestInitThenSetterV5pointSi_Sitvs : $@convention(method) (Int, Int, @inout TestInitThenSetter) -> ()
  // CHECK-NEXT: [[SETTER_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[SETTER_REF]]([[SELF_VALUE:%.*]]) : $@convention(method) (Int, Int, @inout TestInitThenSetter) -> ()
  // CHECK-NEXT: {{.*}} = apply [[SETTER_CLOSURE]]([[ZERO_X]], [[ZERO_Y]]) : $@callee_guaranteed (Int, Int) -> ()
  init(x: Int, y: Int) {
    self.point = (x, y)

    if x == 1 {
      self.point = (0, 0)
    }
  }
}

struct TestPartialInt {
  var x: Int
  var y: Int

  var pointX: Int {
    init(newValue) initializes(x) {
      self.x = newValue
    }

    get { x }
    set { self.x = newValue }
  }

  var pointY: Int {
    init(newValue) initializes(y) {
      self.y = newValue
    }

    get { y }
    set { self.y = newValue }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors14TestPartialIntV1x1yACSi_SitcfC : $@convention(method) (Int, Int, @thin TestPartialInt.Type) -> TestPartialInt
  //
  // CHECK: [[INIT_REF:%.*]] = function_ref @$s14init_accessors14TestPartialIntV6pointXSivi : $@convention(thin) (Int) -> @out Int
  // CHECK: [[X_REF:%.*]] = struct_element_addr {{.*}} : $*TestPartialInt, #TestPartialInt.x
  // CHECK-NEXT: {{.*}} = apply [[INIT_REF]]([[X_REF]], %0) : $@convention(thin) (Int) -> @out Int
  //
  // CHECK: [[INIT_REF:%.*]] = function_ref @$s14init_accessors14TestPartialIntV6pointYSivi : $@convention(thin) (Int) -> @out Int
  // CHECK: [[Y_REF:%.*]] = struct_element_addr {{.*}} : $*TestPartialInt, #TestPartialInt.y
  // CHECK-NEXT: {{.*}} = apply [[INIT_REF]]([[Y_REF]], %1) : $@convention(thin) (Int) -> @out Int
  //
  // CHECK: [[BUILTIN_ONE:%.*]] = integer_literal $Builtin.Int64, 1
  // CHECK-NEXT: [[ONE:%.*]] = struct $Int ([[BUILTIN_ONE]] : $Builtin.Int64)
  // CHECK: [[SETTER_REF:%.*]] = function_ref @$s14init_accessors14TestPartialIntV6pointXSivs : $@convention(method) (Int, @inout TestPartialInt) -> ()
  // CHECK-NEXT: [[SETTER_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[SETTER_REF]]({{.*}}) : $@convention(method) (Int, @inout TestPartialInt) -> ()
  // CHECK-NEXT: {{.*}} = apply [[SETTER_CLOSURE]]([[ONE]]) : $@callee_guaranteed (Int) -> ()
  //
  // CHECK: [[BUILTIN_TWO:%.*]] = integer_literal $Builtin.Int64, 2
  // CHECK-NEXT: [[TWO:%.*]] = struct $Int ([[BUILTIN_TWO]] : $Builtin.Int64)
  // CHECK: [[SETTER_REF:%.*]] = function_ref @$s14init_accessors14TestPartialIntV6pointYSivs : $@convention(method) (Int, @inout TestPartialInt) -> ()
  // CHECK-NEXT: [[SETTER_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[SETTER_REF]]({{.*}}) : $@convention(method) (Int, @inout TestPartialInt) -> ()
  // CHECK-NEXT: {{.*}} = apply [[SETTER_CLOSURE]]([[TWO]]) : $@callee_guaranteed (Int) -> ()
  init(x: Int, y: Int) {
    // Init
    self.pointX = x
    // Init
    self.pointY = y

    // Setter
    self.pointX = 1
    // Setter
    self.pointY = 2

  }
}

struct TestNoInitAndInit {
  var x: Int
  var y: Int

  var pointX: Int {
    init(initalValue) accesses(x) {
    }

    get { x }
    set { }
  }

  var pointY: Int {
    init(initialValue) initializes(y) {
      self.y = initialValue
    }

    get { y }
    set { }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors013TestNoInitAndE0V1x1yACSi_SitcfC : $@convention(method) (Int, Int, @thin TestNoInitAndInit.Type) -> TestNoInitAndInit
  //
  // CHECK: [[INIT_REF:%.*]] = function_ref @$s14init_accessors013TestNoInitAndE0V6pointXSivi : $@convention(thin) (Int, @inout Int) -> ()
  // CHECK: [[SELF_REF:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*TestNoInitAndInit
  // CHECK-NEXT: [[X_REF:%.*]] = struct_element_addr [[SELF_REF]] : $*TestNoInitAndInit, #TestNoInitAndInit.x
  // CHECK-NEXT: {{.*}} = apply [[INIT_REF]](%0, [[X_REF]]) : $@convention(thin) (Int, @inout Int) -> ()
  // CHECK-NEXT: end_access [[SELF_REF]] : $*TestNoInitAndInit
  //
  // CHECK: [[INIT_REF:%.*]] = function_ref @$s14init_accessors013TestNoInitAndE0V6pointYSivi : $@convention(thin) (Int) -> @out Int
  // CHECK: [[SELF_REF:%.*]] = begin_access [modify] [dynamic] {{.*}} : $*TestNoInitAndInit
  // CHECK-NEXT: [[Y_REF:%.*]] = struct_element_addr [[SELF_REF]] : $*TestNoInitAndInit, #TestNoInitAndInit.y
  // CHECK-NEXT: {{.*}} = apply [[INIT_REF]]([[Y_REF]], %1) : $@convention(thin) (Int) -> @out Int
  // CHECK-NEXT: end_access [[SELF_REF]] : $*TestNoInitAndInit
  init(x: Int, y: Int) {
    self.x = x
    self.pointX = x
    self.pointY = y
    print("Point(x: \(self.x), y: \(self.y)")
  }
}

class TestClass {
  var x: Int
  var y: (Int, [String])

  var data: (Int, (Int, [String])) {
    // CHECK-LABEL: sil private @$s14init_accessors9TestClassC4dataSi_Si_SaySSGttvi : $@convention(thin) (Int, Int, @owned Array<String>) -> (@out Int, @out (Int, Array<String>))
    // CHECK: bb0([[X_REF:%.*]] : $*Int, [[Y_REF:%.*]] : $*(Int, Array<String>), [[X_VAL:%.*]] : $Int, [[Y_VAL_0:%.*]] : $Int, [[Y_VAL_1:%.*]] : $Array<String>):
    // CHECK-NEXT: [[Y_VAL_TUPLE:%.*]] = tuple ([[Y_VAL_0]] : $Int, [[Y_VAL_1]] : $Array<String>)

    // CHECK: [[X_ACCESS:%.*]] = begin_access [modify] [static] [[X_REF]] : $*Int
    // CHECK-NEXT: store [[X_VAL]] to [[X_ACCESS]] : $*Int
    // CHECK-NEXT: end_access [[X_ACCESS]] : $*Int
    //
    // CHECK: [[Y_VAL_0:%.*]] = tuple_extract [[Y_VAL_TUPLE]] : $(Int, Array<String>), 0
    // CHECK: [[Y_VAL_1:%.*]] = tuple_extract [[Y_VAL_TUPLE]] : $(Int, Array<String>), 1
    // CHECK: [[Y_ACCESS:%.*]] = begin_access [modify] [static] [[Y_REF]] : $*(Int, Array<String>)
    // CHECK-NEXT: [[Y_ELT_0:%.*]] = tuple_element_addr [[Y_ACCESS]] : $*(Int, Array<String>), 0
    // CHECK-NEXT: store [[Y_VAL_0]] to [[Y_ELT_0]] : $*Int
    // CHECK-NEXT: [[Y_ELT_1:%.*]] = tuple_element_addr [[Y_ACCESS]] : $*(Int, Array<String>), 1
    // CHECK-NEXT: store [[Y_VAL_1]] to [[Y_ELT_1]] : $*Array<String>
    // CHECK-NEXT: end_access [[Y_ACCESS]] : $*(Int, Array<String>)
    init(initialValue) initializes(x y) {
      x = initialValue.0
      y = initialValue.1
    }

    get { (x, y) }
    set {
      x = newValue.0
      y = newValue.1
    }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors9TestClassC1x1yACSi_Si_SaySSGttcfc : $@convention(method) (Int, Int, @owned Array<String>, @owned TestClass) -> @owned TestClass
  // CHECK: [[INIT_ACCESSOR:%.*]] = function_ref @$s14init_accessors9TestClassC4dataSi_Si_SaySSGttvi : $@convention(thin) (Int, Int, @owned Array<String>) -> (@out Int, @out (Int, Array<String>))
  // CHECK: strong_retain [[SELF_REF:%.*]] : $TestClass
  // CHECK: [[X_REF:%.*]] = ref_element_addr [[SELF_REF]] : $TestClass, #TestClass.x
  // CHECK-NEXT: [[Y_REF:%.*]] = ref_element_addr [[SELF_REF]] : $TestClass, #TestClass.y
  // CHECK-NEXT: [[Y_VAL_0:%.*]] = tuple_extract %12 : $(Int, Array<String>), 0
  // CHECK-NEXT: [[Y_VAL_1:%.*]] = tuple_extract %12 : $(Int, Array<String>), 1
  // CHECK-NEXT: {{.*}} = apply [[INIT_ACCESSOR]]([[X_REF]], [[Y_REF]], %0, [[Y_VAL_0]], [[Y_VAL_1]]) : $@convention(thin) (Int, Int, @owned Array<String>) -> (@out Int, @out (Int, Array<String>))
  // CHECK-NEXT: strong_release [[SELF_REF]] : $TestClass
  init(x: Int, y: (Int, [String])) {
    self.data = (x, y)
  }
}

struct TestGeneric<T, U> {
  var a: T
  var b: T
  var c: U

  // CHECK-LABEL: sil private @$s14init_accessors11TestGenericV4datax_xtvi : $@convention(thin) <T, U> (@in T, @in T, @inout U) -> (@out T, @out T)
  //
  // CHECK: bb0([[A_REF:%.*]] : $*T, [[B_REF:%.*]] : $*T, [[A_VALUE:%.*]] : $*T, [[B_VALUE:%.*]] : $*T, [[C_REF:%.*]] : $*U):
  //
  // CHECK: [[A_ACCESS:%.*]] = begin_access [modify] [static] [[A_REF]] : $*T
  // CHECK-NEXT: copy_addr [take] {{.*}} to [init] [[A_ACCESS]] : $*T
  // CHECK-NEXT: end_access [[A_ACCESS]] : $*T
  //
  // CHECK: [[B_ACCESS:%.*]] = begin_access [modify] [static] [[B_REF]] : $*T
  // CHECK-NEXT: copy_addr [take] {{.*}} to [init] [[B_ACCESS]] : $*T
  // CHECK-NEXT: end_access [[B_ACCESS]] : $*T
  //
  // CHECK: [[C_ACCESS:%.*]] = begin_access [read] [static] [[C_REF]] : $*U
  // CHECK-NEXT: [[C_AS_ANY:%.*]] = init_existential_addr {{.*}} : $*Any, $U
  // CHECK-NEXT: copy_addr [[C_ACCESS]] to [init] [[C_AS_ANY]] : $*U
  // CHECK-NEXT: end_access [[C_ACCESS]] : $*U
  var data: (T, T) {
    init(initialValue) initializes(a b) accesses(c) {
      a = initialValue.0
      b = initialValue.1
      print(c)
    }

    get { (a, b) }
    set { }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors11TestGenericV1a1b1cACyxq_Gx_xq_tcfC : $@convention(method) <T, U> (@in T, @in T, @in U, @thin TestGeneric<T, U>.Type) -> @out TestGeneric<T, U>
  //
  // CHECK: [[INIT_ACCESSOR:%.*]] = function_ref @$s14init_accessors11TestGenericV4datax_xtvi : $@convention(thin) <τ_0_0, τ_0_1> (@in τ_0_0, @in τ_0_0, @inout τ_0_1) -> (@out τ_0_0, @out τ_0_0)
  // CHECK: {{.*}} = apply [[INIT_ACCESSOR]]<T, U>({{.*}}) : $@convention(thin) <τ_0_0, τ_0_1> (@in τ_0_0, @in τ_0_0, @inout τ_0_1) -> (@out τ_0_0, @out τ_0_0)
  //
  // CHECK: [[SETTER:%.*]] = function_ref @$s14init_accessors11TestGenericV4datax_xtvs : $@convention(method) <τ_0_0, τ_0_1> (@in τ_0_0, @in τ_0_0, @inout TestGeneric<τ_0_0, τ_0_1>) -> ()
  // CHECK-NEXT: [[SETTER_CLOSURE:%.*]] = partial_apply [callee_guaranteed] [[SETTER]]<T, U>([[SELF_VALUE:%.*]]) : $@convention(method) <τ_0_0, τ_0_1> (@in τ_0_0, @in τ_0_0, @inout TestGeneric<τ_0_0, τ_0_1>) -> ()
  // CHECK: %55 = apply [[SETTER_CLOSURE]]({{.*}}) : $@callee_guaranteed (@in T, @in T) -> ()
  // CHECK-NEXT: end_access [[SELF_VALUE]] : $*TestGeneric<T, U>
  init(a: T, b: T, c: U) {
    self.c = c
    self.data = (a, b)
    self.data = (b, a)
  }
}
