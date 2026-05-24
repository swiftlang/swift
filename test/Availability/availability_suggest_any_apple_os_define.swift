// RUN: %target-swift-frontend -typecheck %s \
// RUN:   -Wwarning UseAnyAppleOSAvailability \
// RUN:   -define-availability 'FourOSesAligned 26:macOS 26, iOS 26, tvOS 26, watchOS 26' \
// RUN:   -diagnostic-style llvm 2>&1 | %FileCheck %s

// CHECK: warning: use '@available(anyAppleOS 26, *)' instead of platform specific '@available' attributes with the same version [#UseAnyAppleOSAvailability]
// CHECK: -define-availability argument:1:55: note: 'allFourOSesAlignedMacro()' is available in watchOS 26 or newer
// CHECK: -define-availability argument:1:43: note: 'allFourOSesAlignedMacro()' is available in tvOS 26 or newer
// CHECK: -define-availability argument:1:34: note: 'allFourOSesAlignedMacro()' is available in iOS 26 or newer
// CHECK: -define-availability argument:1:26: note: 'allFourOSesAlignedMacro()' is available in macOS 26 or newer

@available(FourOSesAligned 26, *)
func allFourOSesAlignedMacro() { }
