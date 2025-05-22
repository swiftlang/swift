// RUN: %target-swift-frontend -print-supported-features | %FileCheck %s

// CHECK: "features": {
// CHECK-NEXT:   "upcoming": [
// CHECK:     { "name": "InferIsolatedConformances", "migratable": true, "categories": ["IsolatedConformances"], "enabled_in": "7" },
// CHECK:   ],
// CHECK:   "experimental": [
// CHECK:     { "name": "{{.*}}" }
// CHECK:   ]
// CHECK: }
