// RUN: %target-swift-frontend -emit-sil -enable-experimental-feature InitAccessors %s | %FileCheck %s

struct TestInit {
  var x: Int
  var y: Int
  var full: (Int, Int)

  var point: (Int, Int) {
    init(initialValue) initializes(y full) accesses(x) {
      self.y = initialValue.1
      self.full = (self.x, self.y)
    }

    get { full }
    set { full = newValue }
  }

  // CHECK-LABEL: sil hidden @$s14init_accessors8TestInitV1x1yACSi_SitcfC : $@convention(method) (Int, Int, @thin TestInit.Type) -> TestInit
  // CHECK: [[SELF_VALUE:%10]] = begin_access [modify] [static] {{.*}} : $*TestInit
  // CHECK-NEXT: // function_ref TestInit.point.init
  // CHECK-NEXT: [[INIT_ACCESSOR:%.*]] = function_ref @$s14init_accessors8TestInitV5pointSi_Sitvi : $@convention(thin) (Int, Int, @inout Int) -> (@out Int, @out (Int, Int))
  // CHECK: [[Y_REF:%.*]] = struct_element_addr [[SELF_VALUE]] : $*TestInit, #TestInit.y
  // CHECK-NEXT: [[FULL_REF:%.*]] = struct_element_addr [[SELF_VALUE]] : $*TestInit, #TestInit.full
  // CHECK-NEXT: [[X_REF:%.*]] = struct_element_addr [[SELF_VALUE]] : $*TestInit, #TestInit.x
  // CHECK-NEXT: {{.*}} = apply [[INIT_ACCESSOR]]([[Y_REF]], [[FULL_REF]], %0, %1, [[X_REF]]) : $@convention(thin) (Int, Int, @inout Int) -> (@out Int, @out (Int, Int))
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
  // CHECK-NEXT: {{.*}} = apply %32([[TWO]]) : $@callee_guaranteed (Int) -> ()
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
  // CHECK: [[X_REF:%.*]] = struct_element_addr {{.*}} : $*TestNoInitAndInit, #TestNoInitAndInit.x
  // CHECK-NEXT: %14 = apply [[INIT_REF]](%0, [[X_REF]]) : $@convention(thin) (Int, @inout Int) -> ()
  //
  // CHECK: [[INIT_REF:%.*]] = function_ref @$s14init_accessors013TestNoInitAndE0V6pointYSivi : $@convention(thin) (Int) -> @out Int
  // CHECK: [[Y_REF:%.*]] = struct_element_addr %16 : $*TestNoInitAndInit, #TestNoInitAndInit.y
  // CHECK-NEXT: %20 = apply [[INIT_REF]]([[Y_REF]], %1) : $@convention(thin) (Int) -> @out Int
  init(x: Int, y: Int) {
    self.x = x
    self.pointX = x
    self.pointY = y
    print("Point(x: \(self.x), y: \(self.y)")
  }
}
