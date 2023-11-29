// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)

// RUN: not %api-digester -deserialize-sdk -input-paths %S/Inputs/diagnostics.json -o - >%t/deserialize.txt 2>&1
// RUN: %FileCheck --input-file %t/deserialize.txt %s
// RUN-X: %FileCheck --input-file %t/deserialize.txt --check-prefix NEGATIVE %s

// RUN: not %api-digester -diagnose-sdk -input-paths %S/Inputs/diagnostics.json -input-paths %S/Inputs/diagnostics-compare.json -compiler-style-diags -o - >%t/diagnose.txt 2>&1
// RUN: %FileCheck --input-file %t/diagnose.txt %s
// RUN: %FileCheck --input-file %t/diagnose.txt --check-prefix NEGATIVE %s

// CHECK: diagnostics.json:5:3: error: unrecognized key 'badKey' in SDK node
// CHECK: diagnostics.json:8:15: error: unrecognized SDK node kind 'Zyzyx'
// CHECK: diagnostics.json:9:41: error: unrecognized type attribute 'fnord' in SDK node
// CHECK: diagnostics.json:9:59: error: unrecognized type attribute 'Available' in SDK node
// CHECK: diagnostics.json:10:39: warning: unrecognized declaration attribute 'Fnord' in SDK node
// CHECK: diagnostics.json:10:56: warning: unrecognized declaration attribute 'inout' in SDK node
// CHECK: diagnostics.json:11:19: error: unrecognized declaration kind 'Subroutine' in SDK node

// Make sure we don't try to output a result:
// NEGATIVE-NOT: "kind": "Root",

// Older versions of the compiler used 'BackDeploy' for @_backDeploy. We now use
// 'BackDeployed', but we should still support the old name.
// NEGATIVE-NOT: {{warning|error}}: unrecognized declaration attribute 'BackDeploy' in SDK node
// NEGATIVE-NOT: is now without @backDeployed

// Should not have any errors in the diagnostics-compare file.
// NEGATIVE-NOT: diagnostics-compare.json:{{.*}}: {{warning|error}}:
