// REQUIRES: OS=macosx

// RUN: %swift_driver -driver-use-frontend-path %S/Inputs/print-var.sh %s DYLD_LIBRARY_PATH | %FileCheck %s

// CHECK: {{^/usr/lib/swift:}}
