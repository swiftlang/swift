// RUN: %sourcekitd-test -repeat-request=5 -req=version | %FileCheck %s
// RUN: %sourcekitd-test -repeat-request=5 -json-request-path %S/Inputs/version_request.json | %FileCheck %s
// CHECK: key.version_major
// CHECK: key.version_major
// CHECK: key.version_major
// CHECK: key.version_major
// CHECK: key.version_major
// CHECK-NOT: key.version_major

// RUN: not %sourcekitd-test -repeat-request=0 -req=version 2>&1 | %FileCheck %s -check-prefix=ERROR
// RUN: not %sourcekitd-test -repeat-request="asdf" -req=version 2>&1 | %FileCheck %s -check-prefix=ERROR
// ERROR: error:
