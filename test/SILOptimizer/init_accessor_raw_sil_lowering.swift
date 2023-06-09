// RUN: %target-swift-frontend -enable-experimental-feature InitAccessors -Xllvm -sil-print-after=definite-init -emit-sil -module-name assign_or_init_lowering %s -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: asserts

// Makes sure that SILGen for memberwise initializer has destroy_addr for overlapping properties.
struct TestMemberwiseWithOverlap {
  var _a: Int
  var _b: Int
  var _c: Int

  var a: Int {
    init(initialValue) initializes(_a)  {
      _a = initialValue
    }

    get { _a }
    set { }
  }

  var pair: (Int, Int) {
    init(initialValue) initializes(_a, _b) {
      _a = initialValue.0
      _b = initialValue.1
    }

    get { (_a, _b) }
    set { }
  }

  var c: Int {
    init(initialValue) initializes(_b, _c) accesses(_a) {
      _b = -1
      _c = initialValue
    }

    get { _c }
    set { }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s23assign_or_init_lowering25TestMemberwiseWithOverlapV1a4pair1cACSi_Si_SitSitcfC : $@convention(method) (Int, Int, Int, Int, @thin TestMemberwiseWithOverlap.Type) -> TestMemberwiseWithOverlap
  //
  // CHECK: [[SELF:%.*]] = alloc_stack $TestMemberwiseWithOverlap
  //
  // CHECK: [[A_REF:%.*]] = struct_element_addr [[SELF]] : $*TestMemberwiseWithOverlap, #TestMemberwiseWithOverlap._a
  // CHECK: [[A_INIT:%.*]] = function_ref @$s23assign_or_init_lowering25TestMemberwiseWithOverlapV1aSivi : $@convention(thin) (Int) -> @out Int
  // CHECK-NEXT: {{.*}} = apply [[A_INIT]]([[A_REF]], %0) : $@convention(thin) (Int) -> @out Int
  //
  // CHECK: [[A_REF:%.*]] = struct_element_addr [[SELF]] : $*TestMemberwiseWithOverlap, #TestMemberwiseWithOverlap._a
  // CHECK-NEXT: destroy_addr [[A_REF]] : $*Int
  // CHECK-NEXT: [[B_REF:%.*]] = struct_element_addr [[SELF]] : $*TestMemberwiseWithOverlap, #TestMemberwiseWithOverlap._b
  // CHECK: [[PAIR_INIT:%.*]] = function_ref @$s23assign_or_init_lowering25TestMemberwiseWithOverlapV4pairSi_Sitvi : $@convention(thin) (Int, Int) -> (@out Int, @out Int)
  // CHECK-NEXT: {{.*}} = apply [[PAIR_INIT]]([[A_REF]], [[B_REF]], %1, %2) : $@convention(thin) (Int, Int) -> (@out Int, @out Int)
  //
  // CHECK: [[B_REF:%.*]] = struct_element_addr [[SELF]] : $*TestMemberwiseWithOverlap, #TestMemberwiseWithOverlap._b
  // CHECK-NEXT: destroy_addr [[B_REF]] : $*Int
  // CHECK-NEXT: [[C_REF:%.*]] = struct_element_addr [[SELF]] : $*TestMemberwiseWithOverlap, #TestMemberwiseWithOverlap._c
  // CHECK-NEXT: [[A_REF:%.*]] = struct_element_addr [[SELF]] : $*TestMemberwiseWithOverlap, #TestMemberwiseWithOverlap._a
  // CHECK: [[C_INIT:%.*]] = function_ref @$s23assign_or_init_lowering25TestMemberwiseWithOverlapV1cSivi : $@convention(thin) (Int, @inout Int) -> (@out Int, @out Int)
  // CHECK-NEXT: {{.*}} = apply [[C_INIT]]([[B_REF]], [[C_REF]], %3, [[A_REF]]) : $@convention(thin) (Int, @inout Int) -> (@out Int, @out Int)
  // CHECK-NEXT: [[RESULT:%.*]] = load [trivial] [[SELF]] : $*TestMemberwiseWithOverlap
  // CHECK-NEXT: dealloc_stack [[SELF]] : $*TestMemberwiseWithOverlap
  // CHECK-NEXT: return [[RESULT]] : $TestMemberwiseWithOverlap
}

_ = TestMemberwiseWithOverlap(a: 0, pair: (1, 2), c: 3)

struct Test1 {
  var _a: Int
  var _b: String

  var a: Int {
    init(initialValue) initializes(_a) {
      _a = initialValue
    }

    get { _a }
    set { }
  }

  var b: String {
    init(initialValue) initializes(_a, _b) {
      _a = 0
      _b = initialValue
    }

    get { _b }
    set { }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s23assign_or_init_lowering5Test1V1aACSi_tcfC : $@convention(method) (Int, @thin Test1.Type) -> @owned Test1
  init(a: Int) {
    // CHECK: assign_or_init [init] [[VALUE:%.*]] : $Int, init {{.*}} : $@convention(thin) (Int) -> @out Int, set {{.*}} : $@callee_guaranteed (Int) -> ()
    self.a = a
    // CHECK: assign_or_init [init] [assign=0] [[VALUE:%.*]] : $String, init {{.*}} : $@convention(thin) (@owned String) -> (@out Int, @out String), set {{.*}} : $@callee_guaranteed (@owned String) -> ()
    self.a = -1
    self.b = ""
    // CHECK: assign_or_init [set] [[VALUE:%.*]] : $Int, init {{.*}} : $@convention(thin) (Int) -> @out Int, set {{.*}} : $@callee_guaranteed (Int) -> ()
    self.a = a
  }
}

struct Test2<T> {
  var _a: Int
  var _b: T
  var _c: String

  var pair: (Int, T) {
    init(initialValue) initializes(_a, _b) {
      _a = initialValue.0
      _b = initialValue.1
    }

    get { (_a, _b) }
    set { }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s23assign_or_init_lowering5Test2V1a1bACyxGSi_xtcfC : $@convention(method) <T> (Int, @in T, @thin Test2<T>.Type) -> @out Test2<T>
  init(a: Int, b: T) {
    // CHECK: assign_or_init [init] [[VALUE:%.*]] : $*(Int, T), init {{.*}} : $@convention(thin) <τ_0_0> (Int, @in τ_0_0) -> (@out Int, @out τ_0_0), set {{.*}} : $@callee_guaranteed (Int, @in T) -> ()
    self.pair = (a, b)
    // CHECK: assign_or_init [init] [assign=0] [assign=1] [[VALUE:%.*]] : $*(Int, T), init {{.*}} : $@convention(thin) <τ_0_0> (Int, @in τ_0_0) -> (@out Int, @out τ_0_0), set {{.*}} : $@callee_guaranteed (Int, @in T) -> ()
    self.pair = (0, b)
    self._c = ""
    // CHECK: assign_or_init [set] [[VALUE:%.*]] : $*(Int, T), init {{.*}} : $@convention(thin) <τ_0_0> (Int, @in τ_0_0) -> (@out Int, @out τ_0_0), set {{.*}} : $@callee_guaranteed (Int, @in T) -> ()
    self.pair = (1, b)
  }
}

struct Test {
  var test: Int {
    init {
    }

    get { 42 }
    set { }
  }

  // CHECK-LABEL: sil hidden [ossa] @$s23assign_or_init_lowering4TestV1vACSi_tcfC : $@convention(method) (Int, @thin Test.Type) -> Test
  init(v: Int) {
    // CHECK: assign_or_init [set] %0 : $Int, init {{.*}} : $@convention(thin) (Int) -> (), set {{.*}} : $@callee_guaranteed (Int) -> ()
    self.test = v
  }
}
