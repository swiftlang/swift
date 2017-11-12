// Check that from invalid input, program suggests using -help

// RUN: not %sourcekitd-test -this_option_does_not_exist 2>&1 | %FileCheck %s

// CHECK: error: unknown argument: -this_option_does_not_exist
// CHECK: Use -h or -help for assistance

