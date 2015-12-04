// RUN: %swift_driver -target x86_64-apple-macosx10.9 -L/foo/ -driver-use-frontend-path %S/Inputs/print-var.sh %s DYLD_LIBRARY_PATH | FileCheck %s
// CHECK: {{^/foo/:[^:]+/lib/swift/macosx$}}
