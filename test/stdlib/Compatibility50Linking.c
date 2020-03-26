// RUN: %empty-directory(%t)
// RUN: %target-clang %s -all_load %test-resource-dir/%target-sdk-name/libswiftCompatibility50.a -lobjc -o %t/main
// RUN: %target-run %t/main
// REQUIRES: objc_interop
// REQUIRES: executable_test

// The compatibility library needs to have no build-time dependencies on
// libswiftCore so it can be linked into a program that doesn't link
// libswiftCore, but will load it at runtime, such as xctest.
//
// Test this by linking it into a plain C program and making sure it builds.

int main(void) {}
