// RUN: %sourcekitd-test -req=version | %FileCheck %s

// CHECK: key.version_major: 1
// CHECK: key.version_minor: 0
