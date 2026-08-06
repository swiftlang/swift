// With assertions enabled the standard library reports a failure through the
// platform's error reporting hook, which is allowed to return, and then traps.
// The message is a literal by the time it reaches the trap, so it is recorded in
// the debug info under the trap as well as printed at runtime. `assert` and
// `assertionFailure` only exist in this configuration.

// RUN: %target-swift-frontend -emit-sil -O     -assert-config Debug -enable-experimental-feature Embedded -wmo -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Osize -assert-config Debug -enable-experimental-feature Embedded -wmo -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -g -O   -assert-config Debug -enable-experimental-feature Embedded -wmo -module-name main %s | %FileCheck --check-prefix CHECK-IR %s

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// Each function is checked for both halves of the failure: the report, which
// prints the message, and the trap that terminates once it returns.

// CHECK-LABEL: sil @$e4main9viaAssertyySiF
// CHECK: function_ref @{{.*}}_embeddedReportFatalErrorInFile
// CHECK: cond_fail {{%.*}}, "assert message"
public func viaAssert(_ x: Int) {
  assert(x >= 0, "assert message")
}

// CHECK-LABEL: sil @$e4main19viaAssertionFailureyySiF
// CHECK: function_ref @{{.*}}_embeddedReportFatalErrorInFile
// CHECK: cond_fail {{%.*}}, "assertionFailure message"
public func viaAssertionFailure(_ x: Int) {
  if x < -1 { assertionFailure("assertionFailure message") }
}

// CHECK-LABEL: sil @$e4main15viaPreconditionyySiF
// CHECK: function_ref @{{.*}}_embeddedReportFatalErrorInFile
// CHECK: cond_fail {{%.*}}, "precondition message"
public func viaPrecondition(_ x: Int) {
  precondition(x > 100, "precondition message")
}

// CHECK-LABEL: sil @$e4main22viaPreconditionFailureyySiF
// CHECK: function_ref @{{.*}}_embeddedReportFatalErrorInFile
// CHECK: cond_fail {{%.*}}, "preconditionFailure message"
public func viaPreconditionFailure(_ x: Int) {
  if x < -7 { preconditionFailure("preconditionFailure message") }
}

// CHECK-LABEL: sil @$e4main13viaFatalErroryySiF
// CHECK: function_ref @{{.*}}_embeddedReportFatalErrorInFile
// CHECK: cond_fail {{%.*}}, "fatalError message"
public func viaFatalError(_ x: Int) {
  if x < -13 { fatalError("fatalError message") }
}

// These messages are unique to this file, so their presence is enough. The
// sibling release-mode test checks that each subprogram belongs to the trap of
// the function it came from.
// CHECK-IR-DAG: !DISubprogram(name: "Swift runtime failure: assert message"
// CHECK-IR-DAG: !DISubprogram(name: "Swift runtime failure: assertionFailure message"
// CHECK-IR-DAG: !DISubprogram(name: "Swift runtime failure: precondition message"
// CHECK-IR-DAG: !DISubprogram(name: "Swift runtime failure: preconditionFailure message"
// CHECK-IR-DAG: !DISubprogram(name: "Swift runtime failure: fatalError message"
