// REQUIRES: OS=linux

// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ %s | %FileCheck %s --check-prefix=DEFAULT-LINUX
// RUN: %swiftc_driver -driver-print-jobs -target x86_64-unknown-linux-gnu -resource-dir %S/Inputs/fake-resource-dir/lib/swift/ -gnustep-objc-interop %s | %FileCheck %s --check-prefix=GNUSTEP-LINUX

// DEFAULT-LINUX: -disable-objc-interop
// DEFAULT-LINUX-NOT: -gnustep-objc-interop

// GNUSTEP-LINUX: -enable-objc-interop
// GNUSTEP-LINUX: -gnustep-objc-interop
// GNUSTEP-LINUX-NOT: -disable-objc-interop

func noop() {}
