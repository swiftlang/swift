// REQUIRES: OS=macosx
// RUN: not %api-digester -deserialize-sdk -input-paths %S/diagnostics.json -o - 2>&1 | %FileCheck %s

// CHECK: diagnostics.json:5:3: error: unrecognized key 'badKey' in SDK node
// CHECK: diagnostics.json:8:15: error: unrecognized SDK node kind 'Zyzyx'
// CHECK: diagnostics.json:9:41: error: unrecognized type attribute 'fnord' in SDK node
// CHECK: diagnostics.json:9:59: error: unrecognized type attribute 'Available' in SDK node
// CHECK: diagnostics.json:10:39: error: unrecognized declaration attribute 'Fnord' in SDK node
// CHECK: diagnostics.json:10:56: error: unrecognized declaration attribute 'inout' in SDK node
// CHECK: diagnostics.json:11:19: error: unrecognized declaration kind 'Subroutine' in SDK node

// Make sure we don't try to output a result:
// CHECK-NOT: "kind": "Root",
