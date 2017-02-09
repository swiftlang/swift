// Note: This deliberately uses the script interpreter rather than build/run.
// RUN: %swift_driver -import-objc-header %S/Inputs/availability_host_os.h -DFAIL -Xfrontend -verify %s
// RUN: %swift_driver -import-objc-header %S/Inputs/availability_host_os.h %s | %FileCheck %s
// RUN: not %swift -typecheck -import-objc-header %S/Inputs/availability_host_os.h %s 2>&1 | %FileCheck -check-prefix=CHECK-NOT-INFERRED %s

// REQUIRES: OS=macosx
// REQUIRES: executable_test

print(mavericks()) // CHECK: {{^9$}}
print(yosemite()) // CHECK-NEXT: {{^10$}}
// CHECK-NOT-INFERRED: 'yosemite()' is only available on OS X 10.10 or newer

#if FAIL
print(todosSantos()) // expected-error {{'todosSantos()' is only available on OS X 10.99 or newer}}
// expected-note@-1 {{add 'if #available' version check}}
#endif
