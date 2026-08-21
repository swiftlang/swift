// The Embedded `StaticString` overloads of `fatalError`, `precondition`, and
// `preconditionFailure` pass the caller's message to
// `Builtin.condfail_message`, so a literal message reaches the `cond_fail` and,
// from there, the name of the trap's artificial subprogram in the debug
// info. In release configurations that is the only place the message survives.

// RUN: %target-swift-frontend -emit-sil -O     -enable-experimental-feature Embedded -wmo -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-sil -Osize -enable-experimental-feature Embedded -wmo -module-name main %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-ir -g -O   -enable-experimental-feature Embedded -wmo -module-name main %s | %FileCheck --check-prefix CHECK-IR %s

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

// Each function traps on a different condition to avoid function merging.

// CHECK-LABEL: sil @$e4main12fatalMessageyySiF
// CHECK: cond_fail {{%.*}}, "fatalError message"
// CHECK-IR-LABEL: define {{.*}}@"$e4main12fatalMessageyySiF"
// CHECK-IR: call void @llvm.trap(){{.*}}!dbg ![[FATAL_LOC:[0-9]+]]
public func fatalMessage(_ x: Int) {
  if x < 0 { fatalError("fatalError message") }  // LINE 21
}

// CHECK-LABEL: sil @$e4main19preconditionMessageyySiF
// CHECK: cond_fail {{%.*}}, "precondition message"
// CHECK-IR-LABEL: define {{.*}}@"$e4main19preconditionMessageyySiF"
// CHECK-IR: call void @llvm.trap(){{.*}}!dbg ![[PRECONDITION_LOC:[0-9]+]]
public func preconditionMessage(_ x: Int) {
  precondition(x > 100, "precondition message")  // LINE 29
}

// CHECK-LABEL: sil @$e4main26preconditionFailureMessageyySiF
// CHECK: cond_fail {{%.*}}, "preconditionFailure message"
// CHECK-IR-LABEL: define {{.*}}@"$e4main26preconditionFailureMessageyySiF"
// CHECK-IR: call void @llvm.trap(){{.*}}!dbg ![[PRECONDITION_FAILURE_LOC:[0-9]+]]
public func preconditionFailureMessage(_ x: Int) {
  if x < -7 { preconditionFailure("preconditionFailure message") }  // LINE 37
}

// Messages that aren't literals are replaced with a generic error message.

// CHECK-LABEL: sil @$e4main15fallbackMessageyySi_s12StaticStringVtF
// CHECK: builtin "condfail_message"
// CHECK-IR-LABEL: define {{.*}}@"$e4main15fallbackMessageyySi_s12StaticStringVtF"
// CHECK-IR: call void @llvm.trap(){{.*}}!dbg ![[FALLBACK_LOC:[0-9]+]]
public func fallbackMessage(_ x: Int, _ message: StaticString) {
  if x < -13 { fatalError(message) }  // LINE 47
}

// CHECK-IR-DAG: ![[FATAL_LOC]] = !DILocation(line: 0, scope: ![[FATAL:[0-9]+]], inlinedAt: ![[FATAL_CALL:[0-9]+]])
// CHECK-IR-DAG: ![[FATAL]] = distinct !DISubprogram(name: "Swift runtime failure: fatalError message"
// CHECK-IR-DAG: ![[FATAL_CALL]] = !DILocation(line: 21, column: {{[0-9]+}},

// CHECK-IR-DAG: ![[PRECONDITION_LOC]] = !DILocation(line: 0, scope: ![[PRECONDITION:[0-9]+]], inlinedAt: ![[PRECONDITION_CALL:[0-9]+]])
// CHECK-IR-DAG: ![[PRECONDITION]] = distinct !DISubprogram(name: "Swift runtime failure: precondition message"
// CHECK-IR-DAG: ![[PRECONDITION_CALL]] = !DILocation(line: 29, column: {{[0-9]+}},

// CHECK-IR-DAG: ![[PRECONDITION_FAILURE_LOC]] = !DILocation(line: 0, scope: ![[PRECONDITION_FAILURE:[0-9]+]], inlinedAt: ![[PRECONDITION_FAILURE_CALL:[0-9]+]])
// CHECK-IR-DAG: ![[PRECONDITION_FAILURE]] = distinct !DISubprogram(name: "Swift runtime failure: preconditionFailure message"
// CHECK-IR-DAG: ![[PRECONDITION_FAILURE_CALL]] = !DILocation(line: 37, column: {{[0-9]+}},

// CHECK-IR-DAG: ![[FALLBACK_LOC]] = !DILocation(line: 0, scope: ![[FALLBACK:[0-9]+]], inlinedAt: ![[FALLBACK_CALL:[0-9]+]])
// CHECK-IR-DAG: ![[FALLBACK]] = distinct !DISubprogram(name: "Swift runtime failure: unknown program error"
// CHECK-IR-DAG: ![[FALLBACK_CALL]] = !DILocation(line: 47, column: {{[0-9]+}},
