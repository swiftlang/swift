// RUN: %swift -constraint-checker -emit-llvm %s | FileCheck %s

// CHECK: [[TYPE:%swift.type]] = type
// CHECK: [[B:%C12super_method1B]] = type
// CHECK: [[D:%C12super_method1D]] = type

class B {
  func frob() { }
  static func runce() { }
}

class D : B {
  // CHECK: define void @_TC12super_method1D4frobfS0_FT_T_([[D]]* %this) {
  func frob() {
    // CHECK: call void @_TC12super_method1B4frobfS0_FT_T_([[B]]* {{.*}})
    super.frob()
  }
  // CHECK: }

  // CHECK: define void @_TC12super_method1D5runcefMS0_FT_T_([[TYPE]]* %this) {
  static func runce() {
    // CHECK: call void @_TC12super_method1B5runcefMS0_FT_T_([[TYPE]]* {{.*}})
    super.runce()
  }
}

