// RUN: not %target-swift-frontend -parse %s 2>&1 | FileCheck -strict-whitespace %s

// Test the diagnostic option 'PointsToFirstBadToken'.

typealias TestDiagPointsToEndOfLine =

// CHECK: error: expected type in typealias declaration
// CHECK-NEXT: {{^}}typealias TestDiagPointsToEndOfLine ={{$}}
// CHECK-NEXT: {{^}}                                     ^{{$}}

typealias TestDiagPointsToBadToken = =

// CHECK: error: expected type in typealias declaration
// CHECK-NEXT: {{^}}typealias TestDiagPointsToBadToken = ={{$}}
// CHECK-NEXT: {{^}}                                     ^{{$}}

