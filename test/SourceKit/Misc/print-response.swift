// RUN: %sourcekitd-test -req=version | not %FileCheck %s
// RUN: %sourcekitd-test -json-request-path %S/Inputs/version_request.json | not %FileCheck %s
// RUN: %sourcekitd-test -req=version -dont-print-response | %FileCheck %s -allow-empty
// RUN: %sourcekitd-test -json-request-path %S/Inputs/version_request.json -dont-print-response | %FileCheck %s -allow-empty
// CHECK-NOT: key.version_major
