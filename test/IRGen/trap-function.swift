// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend -primary-file %s -trap-function oopsie -emit-ir -module-name trap_function -I %t | %FileCheck %s -check-prefix=TRAPFN
// RUN: %target-swift-frontend -O -primary-file %s -trap-function oopsie -emit-ir -module-name trap_function -I %t | %FileCheck %s -check-prefix=TRAPFN_OPT
// RUN: %target-swift-frontend -primary-file %s -emit-ir -module-name trap_function -I %t | %FileCheck %s -check-prefix=NOTRAPFN
// REQUIRES: objc_codegen

// TRAPFN-LABEL: define hidden swiftcc {{.*}} @"$s13trap_function14checkOverflow1yS2i_SitF"
// TRAPFN: call void @llvm.trap() [[ATTR0:#[0-9]+]]

// TRAPFN_OPT-LABEL: define hidden swiftcc {{.*}} @"$s13trap_function14checkOverflow1yS2i_SitF"
// TRAPFN_OPT: call void @llvm.trap() [[ATTR0:#[0-9]+]]

// NOTRAPFN-LABEL: define hidden swiftcc {{.*}} @"$s13trap_function14checkOverflow1yS2i_SitF"
// NOTRAPFN: call void @llvm.trap(){{$}}
func checkOverflow1(_ a: Int, _ b: Int) -> Int {
  a + b
}

// TRAPFN-LABEL: define hidden swiftcc {{.*}} @"$s13trap_function14checkOverflow2yySiz_SitF"
// TRAPFN: call void @llvm.trap() [[ATTR0:#[0-9]+]]

// TRAPFN_OPT-LABEL: define hidden swiftcc {{.*}} @"$s13trap_function14checkOverflow2yySiz_SitF"
// TRAPFN_OPT: call void @llvm.trap() [[ATTR0:#[0-9]+]]

// NOTRAPFN-LABEL: define hidden swiftcc {{.*}} @"$s13trap_function14checkOverflow2yySiz_SitF"
// NOTRAPFN: call void @llvm.trap(){{$}}
func checkOverflow2(_ a: inout Int, _ b: Int) {
  a *= b
}

// TRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function17checkPreconditionyySiF"
// TRAPFN: call swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"
// TRAPFN-NOT: call void @llvm.trap()

// TRAPFN_OPT-LABEL: define hidden swiftcc void @"$s13trap_function17checkPreconditionyySiF"
// TRAPFN_OPT: call void @llvm.trap() [[ATTR0]]

// NOTRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function17checkPreconditionyySiF"
// NOTRAPFN: call swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"
func checkPrecondition(_ a: Int) {
  precondition(a != 23)
}

// TRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function24checkPreconditionFailureyySiF"
// TRAPFN: call swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"
// TRAPFN-NOT: call void @llvm.trap()

// TRAPFN_OPT-LABEL: define hidden swiftcc void @"$s13trap_function24checkPreconditionFailureyySiF"
// TRAPFN_OPT: call void @llvm.trap() [[ATTR0]]

// NOTRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function24checkPreconditionFailureyySiF"
// NOTRAPFN: call swiftcc void @"$ss17_assertionFailure__4file4line5flagss5NeverOs12StaticStringV_SSAHSus6UInt32VtF"
func checkPreconditionFailure(_ a: Int) {
  if a == 42 {
    preconditionFailure()
  }
}

// TRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function10terminatoryyF"
// TRAPFN_OPT-LABEL: define hidden swiftcc void @"$s13trap_function10terminatoryyF"
// NOTRAPFN-LABEL: define hidden swiftcc void @"$s13trap_function10terminatoryyF"
func terminator() {
}

// TRAPFN: attributes [[ATTR0]] = { {{.*}}"trap-func-name"="oopsie" }
// TRAPFN_OPT: attributes [[ATTR0]] = { {{.*}}"trap-func-name"="oopsie" }
// NOTRAPFN-NOT: attributes {{.*}} = { {{.*}}"trap-func-name"="oopsie" }
