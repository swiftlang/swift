// RUN: %target-swift-frontend -print-target-info | %FileCheck %s

// CHECK: "feature_flags": {
// CHECK-NEXT:   "upcoming": [
// CHECK:     { "name": "{{.*}}"{{, "adoptable": true}}, "introduced_in": {{[0-9]+}} }
// CHECK:   ],
// CHECK-NEXT:   "experimental": [
// CHECK:     { "name": "{{.*}}" }
// CHECK:   ]
// CHECK: }
