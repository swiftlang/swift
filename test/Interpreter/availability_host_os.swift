// Note: This is deliberately testing the target-less invocation of swift(c).

// RUN: %swiftc_driver -typecheck -import-objc-header %S/Inputs/availability_host_os.h -DFAIL -Xfrontend -verify %s
// RUN: %swift_driver -import-objc-header %S/Inputs/availability_host_os.h -DFAIL -Xfrontend -verify %s

// RUN: %swift_driver -import-objc-header %S/Inputs/availability_host_os.h %s | %FileCheck %s


// REQUIRES: OS=macosx
// REQUIRES: executable_test

print(mavericks()) // CHECK: {{^9$}}
print(yosemite()) // CHECK-NEXT: {{^10$}}

#if FAIL
print(todosSantos()) // expected-error {{'todosSantos()' is only available on OS X 10.99 or newer}}
// expected-note@-1 {{add 'if #available' version check}}
#endif
