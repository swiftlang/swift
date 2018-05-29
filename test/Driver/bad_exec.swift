// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: not %swiftc_driver -driver-use-frontend-path /always/searching/never/finding %s 2>&1 | %FileCheck %s
// CHECK: unable to execute command
func thing() {
    print(1)
}
