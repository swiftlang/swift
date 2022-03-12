// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Debug -Onone
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Release -O
// RUN: %target-build-swift %s -Xfrontend -disable-access-control -o %t/Assert_Unchecked -Ounchecked
// RUN: %target-codesign %t/Assert_Debug
// RUN: %target-codesign %t/Assert_Release
// RUN: %target-codesign %t/Assert_Unchecked
// RUN: %target-run %t/Assert_Debug | %FileCheck --check-prefixes=DEBUG %s
// RUN: %target-run %t/Assert_Release | %FileCheck --check-prefixes=RELEASE %s
// RUN: %target-run %t/Assert_Unchecked | %FileCheck --check-prefixes=UNCHECKED %s

// REQUIRES: swift_stdlib_debug_preconditions_in_release

// DEBUG: _isStdlibDebugChecksEnabled: true
// RELEASE: _isStdlibDebugChecksEnabled: true
// UNCHECKED: _isStdlibDebugChecksEnabled: false
print("_isStdlibDebugChecksEnabled: \(_isStdlibDebugChecksEnabled())")


func check() -> Bool {
  print("Debug preconditions are active")
  return true
}

// DEBUG-NEXT: Debug preconditions are active
// RELEASE-NEXT: Debug preconditions are active
// UNCHECKED-NOT: Debug preconditions are active
_debugPrecondition(check()) // Note: side effects in an assert are a terrible
                            // idea; do not emulate this pattern in real code.

// DEBUG-NEXT: Done
// RELEASE-NEXT: Done
// UNCHECKED-NEXT: Done
print("Done")
