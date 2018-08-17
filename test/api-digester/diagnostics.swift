// REQUIRES: OS=macosx
// RUN: not %api-digester -deserialize-sdk -input-paths %S/diagnostics.json -o - 2>&1 | %FileCheck %s

// CHECK: diagnostics.json:6:3: error: unrecognized key 'badKey' in SDK node

// Make sure we don't try to output a result:
// CHECK-NOT: "kind": "Root",
