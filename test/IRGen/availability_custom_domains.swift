// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -Onone | %FileCheck %s

// RUN: %target-swift-emit-irgen -module-name Test %s -verify \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-always-enabled-availability-domain AlwaysEnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -O | %FileCheck %s

// REQUIRES: swift_feature_CustomAvailability

@_silgen_name("always")
public func always()

@_silgen_name("never")
public func never()

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test24ifAvailableEnabledDomainyyF"()
// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
public func ifAvailableEnabledDomain() {
  if #available(EnabledDomain) {
    always()
  } else {
    never()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test30ifAvailableAlwaysEnabledDomainyyF"()
// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
public func ifAvailableAlwaysEnabledDomain() {
  if #available(AlwaysEnabledDomain) {
    always()
  } else {
    never()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test25ifAvailableDisabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func ifAvailableDisabledDomain() {
  if #available(DisabledDomain) {
    never()
  } else {
    always()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test26ifUnavailableEnabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func ifUnavailableEnabledDomain() {
  if #unavailable(EnabledDomain) {
    never()
  } else {
    always()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test32ifUnavailableAlwaysEnabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func ifUnavailableAlwaysEnabledDomain() {
  if #unavailable(EnabledDomain) {
    never()
  } else {
    always()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test27ifUnavailableDisabledDomainyyF"()
// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
public func ifUnavailableDisabledDomain() {
  if #unavailable(DisabledDomain) {
    always()
  } else {
    never()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test28whileAvailableDisabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
public func whileAvailableDisabledDomain() {
  while #available(DisabledDomain) {
    never()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test29whileUnavailableEnabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
public func whileUnavailableEnabledDomain() {
  while #unavailable(EnabledDomain) {
    never()
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test27guardAvailableEnabledDomainyyF"()
// CHECK: call swiftcc void @always()
// CHECK-NOT: call swiftcc void @never()
public func guardAvailableEnabledDomain() {
  guard #available(EnabledDomain) else {
    never()
    return
  }
  always()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test28guardAvailableDisabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func guardAvailableDisabledDomain() {
  guard #available(DisabledDomain) else {
    always()
    return
  }
  never()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test29guardUnavailableEnabledDomainyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: call swiftcc void @always()
public func guardUnavailableEnabledDomain() {
  guard #unavailable(EnabledDomain) else {
    always()
    return
  }
  never()
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF"()
// CHECK-NOT: call swiftcc void @never()
public func testIfAvailableDisabledDomainNestedDecls() {
  if #available(DisabledDomain) {
    func nestedFunc() { never()}
    let nestedClosure = { never() }
    struct NestedStruct {
      func m() { never() }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
  }
}

// CHECK-NOT: define internal swiftcc void @"$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF"
// CHECK-NOT: define internal swiftcc void @"$s4Test40testIfAvailableDisabledDomainNestedDeclsyyFyycfU_"

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF"()
// CHECK-NOT: call swiftcc void @never()
public func testIfAvailableEnabledDomainElseNestedDecls() {
  if #available(EnabledDomain) {
  } else {
    func nestedFunc() { never()}
    let nestedClosure = { never() }
    struct NestedStruct {
      func m() { never() }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
  }
}

// CHECK-NOT: define internal swiftcc void @"$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF10nestedFuncL_yyF"
// CHECK-NOT: define internal swiftcc void @"$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyFyycfU_"

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF"()
// CHECK-NOT: call swiftcc void @never()
public func testWhileAvailableDisabledDomainNestedDecls() {
  while #available(DisabledDomain) {
    func nestedFunc() { never() }
    let nestedClosure = { never() }
    struct NestedStruct {
      func m() { never() }
    }
    nestedFunc()
    nestedClosure()
    NestedStruct().m()
  }
}

// CHECK-NOT: define internal swiftcc void @"$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF"
// CHECK-NOT: define internal swiftcc void @"$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyFyycfU_"

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test46testIfAvailableDisabledDomainLocalVarAccessorsyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK-NOT: $s4Test46testIfAvailableDisabledDomainLocalVarAccessorsyyF{{[0-9]}}
public func testIfAvailableDisabledDomainLocalVarAccessors() {
  if #available(DisabledDomain) {
    var computed: Int { never(); return 0 }
    var observed: Int = 0 { didSet { never() } }
    var explicitAccessors: Int {
      get { never(); return 0 }
      set { if newValue == 0 { never() } }
    }
    observed = computed
    explicitAccessors = observed
    _ = explicitAccessors
  }
}

// CHECK-LABEL: define {{.*}}swiftcc void @"$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF"()
// CHECK-NOT: call swiftcc void @never()
// CHECK: ret void
public func testGuardAvailableDisabledDomainNestedDecls() {
  guard #available(DisabledDomain) else { return }
  func nestedFunc() { never() }
  let nestedClosure = { never() }
  struct NestedStruct {
    func m() { never() }
  }
  nestedFunc()
  nestedClosure()
  NestedStruct().m()
}

// CHECK-NOT: define internal swiftcc void @"$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyFyycfU_"
// CHECK-NOT: define internal swiftcc void @"$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF10nestedFuncL_yyF"

// CHECK-NOT: define internal swiftcc void @"$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF"
// CHECK-NOT: define internal swiftcc void @"$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF0H6StructL_V1myyF"
// CHECK-NOT: define internal swiftcc void @"$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF"
// CHECK-NOT: define internal swiftcc void @"$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF0G6StructL_V1myyF"

// CHECK-NOT: @"$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF0G6StructL_VMa"
// CHECK-NOT: @"$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF0H6StructL_VMa"
// CHECK-NOT: @"$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF0G6StructL_VMa"
// CHECK-NOT: @"$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF0G6StructL_VMa"
// CHECK-NOT: @"$s4Test40testIfAvailableDisabledDomainNestedDeclsyyF0G6StructL_VMn"
// CHECK-NOT: @"$s4Test43testIfAvailableEnabledDomainElseNestedDeclsyyF0H6StructL_VMn"
// CHECK-NOT: @"$s4Test43testWhileAvailableDisabledDomainNestedDeclsyyF0G6StructL_VMn"
// CHECK-NOT: @"$s4Test43testGuardAvailableDisabledDomainNestedDeclsyyF0G6StructL_VMn"
