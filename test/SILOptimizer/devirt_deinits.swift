// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -primary-file %s -parse-as-library -O -sil-verify-all -module-name=test -Xllvm -sil-disable-pass=function-signature-opts -emit-sil | %FileCheck %s

// Also do an end-to-end test and check if the compiled executable works as expected.
// RUN: %target-run-simple-swift(-target %target-cpu-apple-macos14 -parse-as-library -O -Xllvm -sil-disable-pass=function-signature-opts) | %FileCheck -check-prefix CHECK-OUTPUT %s

// Check if it works in embedded mode.
// RUN: %target-run-simple-swift(-target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck -check-prefix CHECK-OUTPUT %s

// Run without the deinit-devirtualizer to verify that our CHECK-OUTPUT lines are correct.
// TODO: currently disabled because of rdar://118449507
// RUNx: %target-run-simple-swift(-target %target-cpu-apple-macos14 -Xllvm -sil-disable-pass=deinit-devirtualizer -parse-as-library) | %FileCheck -check-prefix CHECK-OUTPUT %s


// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// REQUIRES: swift_in_compiler
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

// UNSUPPORTED: use_os_stdlib

@inline(never)
func log(_ s: StaticString) {
  print(s)
}

struct S1: ~Copyable {
  @inline(never)
  deinit {
    log("deinit S1")
  }
}

struct S2: ~Copyable {
  @inline(never)
  deinit {
    log("deinit S2")
  }
}

struct S3: ~Copyable {
  let a = S1()

  @inline(never)
  deinit {
    log("deinit S3")
  }
}

enum EnumWithDeinit: ~Copyable {
  case A(Int)
  case B
  case C(S1)
}

enum E1: ~Copyable {
  case A(Int)
  case B
  case C(S1)
}

enum E2: ~Copyable {
  case A
  case B(E1)
  case C(S2)
}

struct StrWithoutDeinit: ~Copyable {
  var a = S1()
  var b = S2()
  let c = 27
}

// CHECK-LABEL: sil hidden [noinline] @$s4test2S3VfD :
// CHECK:         [[D:%.*]] = function_ref @$s4test2S1VfD :
// CHECK:         apply [[D]]
// CHECK:       } // end sil function '$s4test2S3VfD'

// CHECK-LABEL: sil hidden [noinline] @$s4test0A6SimpleyyAA2S1VnF :
// CHECK:         [[D:%.*]] = function_ref @$s4test2S1VfD :
// CHECK:         apply [[D]]
// CHECK:       } // end sil function '$s4test0A6SimpleyyAA2S1VnF'
@inline(never)
func testSimple(_ s: consuming S1) {
  log("### testSimple")
}

// CHECK-LABEL: sil hidden [noinline] @$s4test0A15TwoFieldDeinitsyyAA16StrWithoutDeinitVnF :
// CHECK:         [[D1:%.*]] = function_ref @$s4test2S1VfD :
// CHECK:         apply [[D1]]
// CHECK:         [[D2:%.*]] = function_ref @$s4test2S2VfD :
// CHECK:         apply [[D2]]
// CHECK:       } // end sil function '$s4test0A15TwoFieldDeinitsyyAA16StrWithoutDeinitVnF'
@inline(never)
func testTwoFieldDeinits(_ s: consuming StrWithoutDeinit) {
  log("### testTwoFieldDeinits")
}

// CHECK-LABEL: sil hidden [noinline] @$s4test0A5Enum1yyAA2E1OnF :
// CHECK:         switch_enum
// CHECK:       bb1({{.*}}):
// CHECK:         [[D:%.*]] = function_ref @$s4test2S1VfD :
// CHECK:         apply [[D]]
// CHECK:       } // end sil function '$s4test0A5Enum1yyAA2E1OnF'
@inline(never)
func testEnum1(_ s: consuming E1) {
  log("### testEnum1")
}

// CHECK-LABEL: sil hidden [noinline] @$s4test0A5Enum2yyAA2E2OnF :
// CHECK:         switch_enum
// CHECK:       bb1({{.*}}):
// CHECK:         [[D:%.*]] = function_ref @$s4test2S2VfD :
// CHECK:         apply [[D]]
// CHECK:       bb2({{.*}}):
// CHECK:         switch_enum
// CHECK:       bb3({{.*}}):
// CHECK:         [[D:%.*]] = function_ref @$s4test2S1VfD :
// CHECK:         apply [[D]]
// CHECK:       } // end sil function '$s4test0A5Enum2yyAA2E2OnF'
@inline(never)
func testEnum2(_ s: consuming E2) {
  log("### testEnum2")
}

// CHECK-LABEL: sil hidden [noinline] @$s4test0A12NestedDeinityyAA2S3VnF :
// CHECK:         [[D:%.*]] = function_ref @$s4test2S3VfD :
// CHECK:         apply [[D]]
// CHECK:       } // end sil function '$s4test0A12NestedDeinityyAA2S3VnF'
@inline(never)
func testNestedDeinit(_ s: consuming S3) {
  log("### testNestedDeinit")
}

@main
struct Main {
  static func main() {

    // CHECK-OUTPUT-LABEL: ### testSimple
    // CHECK-OUTPUT-NEXT:  deinit S1
    // CHECK-OUTPUT-NEXT:  ---
    testSimple(S1())
    log("---")

    // CHECK-OUTPUT-LABEL: ### testTwoFieldDeinits
    // CHECK-OUTPUT-NEXT:  deinit S1
    // CHECK-OUTPUT-NEXT:  deinit S2
    // CHECK-OUTPUT-NEXT:  ---
    testTwoFieldDeinits(StrWithoutDeinit())
    log("---")

    // CHECK-OUTPUT-LABEL: ### testEnum1
    // CHECK-OUTPUT-NEXT:  ---
    testEnum1(E1.A(27))
    log("---")

    // CHECK-OUTPUT-LABEL: ### testEnum1
    // CHECK-OUTPUT-NEXT:  deinit S1
    // CHECK-OUTPUT-NEXT:  ---
    testEnum1(E1.C(S1()))
    log("---")

    // CHECK-OUTPUT-LABEL: ### testEnum2
    // CHECK-OUTPUT-NEXT:  deinit S1
    // CHECK-OUTPUT-NEXT:  ---
    testEnum2(E2.B(E1.C(S1())))
    log("---")

    // CHECK-OUTPUT-LABEL: ### testEnum2
    // CHECK-OUTPUT-NEXT:  deinit S2
    // CHECK-OUTPUT-NEXT:  ---
    testEnum2(E2.C(S2()))
    log("---")

    // CHECK-OUTPUT-LABEL: ### testNestedDeinit
    // CHECK-OUTPUT-NEXT:  deinit S3
    // CHECK-OUTPUT-NEXT:  deinit S1
    // CHECK-OUTPUT-NEXT:  ---
    testNestedDeinit(S3())
    log("---")
  }
}

