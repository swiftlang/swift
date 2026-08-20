// RUN: %empty-directory(%t)

// RUN: mkdir -p %t/swift-root/libexec/swift
// RUN: touch %t/swift-root/libexec/swift/Foo

// RUN: %target-build-swift -enable-experimental-feature Extern %s -o %t/privilege-test

// A test binary is signed with get-task-allow, so it is neither restricted nor
// restricted-for-exec and every variable applies.
// RUN: %target-codesign %t/privilege-test
// RUN: env %env-SWIFT_ROOT=%t/swift-root %target-run %t/privilege-test | %FileCheck %s --check-prefix CHECK-UNRESTRICTED

// Dropping get-task-allow leaves the process debuggable-restricted only, which
// gates SWIFT_ROOT but not the checks. This is what a plain `swiftc` build
// gets, so the check-weakening variables should keep working here.
// RUN: codesign -f -s - %t/privilege-test
// RUN: env %env-SWIFT_ROOT=%t/swift-root %target-run %t/privilege-test | %FileCheck %s --check-prefix CHECK-NO-TASK-ALLOW

// The hardened runtime sets CS_RUNTIME, which gates the check-weakening
// variables too. Library validation has to be off for the test binary to load
// the just-built runtime.
// RUN: codesign -f -s - --options runtime --entitlements %S/Inputs/hardened-runtime.plist %t/privilege-test
// RUN: env %env-SWIFT_ROOT=%t/swift-root %target-run %t/privilege-test | %FileCheck %s --check-prefix CHECK-HARDENED

// REQUIRES: executable_test
// REQUIRES: swift_feature_Extern
// REQUIRES: OS=macosx

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

@_extern(c, "_swift_isRestrictedProcess")
func _swift_isRestrictedProcess() -> CBool

@_extern(c, "swift_getRootPath")
func swift_getRootPath() -> UnsafePointer<CChar>?

// Gates the variables that disable a check. A plain `swiftc` build lands in
// the CHECK-NO-TASK-ALLOW configuration and must report no.
// CHECK-UNRESTRICTED: restricted: no
// CHECK-NO-TASK-ALLOW: restricted: no
// CHECK-HARDENED: restricted: yes
print("restricted: \(_swift_isRestrictedProcess() ? "yes" : "no")")

// CHECK-UNRESTRICTED: SWIFT_ROOT honored: yes
// CHECK-NO-TASK-ALLOW: SWIFT_ROOT honored: no
// CHECK-HARDENED: SWIFT_ROOT honored: no
let rootPath = swift_getRootPath().map { String(cString: $0) }
print("SWIFT_ROOT honored: \(rootPath?.contains("swift-root") == true ? "yes" : "no")")
