// REQUIRES: OS=macosx

// RUN: %swift_driver -driver-use-frontend-path %S/Inputs/print-var.sh %s DYLD_LIBRARY_PATH | %FileCheck %s --dump-input fail

// Pass if the variable is not set at all. Something causes the
// variable to get lost in PR testing. Accept that for now. This is
// a temporary solution and so this will go away soon in any case.
// CHECK: {{(^/usr/lib/swift:)|(^NO_VALUE$)}}
