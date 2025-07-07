// RUN: %target-swift-frontend -print-supported-features | %FileCheck %s

// CHECK: "features": {
// CHECK-NEXT:   "optional": [
// CHECK: { "name": "StrictMemorySafety", "migratable": true, "categories": ["StrictMemorySafety"], "flag_name": "-strict-memory-safety" }
// CHECK-NEXT: ],
// CHECK-NEXT:   "upcoming": [
// CHECK:     { "name": "InferIsolatedConformances", "migratable": true, "categories": ["IsolatedConformances"], "enabled_in": "7" },
// CHECK:   ],
// CHECK:   "experimental": [
// CHECK:     { "name": "{{.*}}" }
// CHECK:   ]
// CHECK: }
