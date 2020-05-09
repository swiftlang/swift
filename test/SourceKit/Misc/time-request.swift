// RUN: %sourcekitd-test -time-request -req=version 2>&1 | %FileCheck %s
// RUN: %sourcekitd-test -time-request -json-request-path %S/Inputs/version_request.json 2>&1 | %FileCheck %s
// CHECK-DAG: key.version_major
// CHECK-DAG: request time: {{.*}} ms
