// RUN: %target-swift-frontend -print-supported-features | %FileCheck %s

// CHECK: "features": {
// CHECK-NEXT:   "upcoming": [
// CHECK:     { "name": "{{.*}}"{{, "migratable": true}}, "enabled_in": {{[0-9]+}} }
// CHECK:   ],
// CHECK-NEXT:   "experimental": [
// CHECK:     { "name": "{{.*}}" }
// CHECK:   ]
// CHECK: }
