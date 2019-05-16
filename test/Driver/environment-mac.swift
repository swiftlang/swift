// REQUIRES: OS=macosx

// RUN: %swift_driver -driver-use-frontend-path %S/Inputs/print-var.sh %s DYLD_LIBRARY_PATH | %FileCheck %s --dump-input fail

// CHECK: {{^/usr/lib/swift:}}
