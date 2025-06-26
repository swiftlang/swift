// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -debug-constraints -verify %s 2>%t.err
// RUN: %FileCheck %s < %t.err

// REQUIRES: objc_interop

// rdar://153461854

import CoreGraphics

// There is no better way to check this at the moment because diagnostic mode would produce
// a valid solution for this example. We need to make sure that the solution is produced in
// normal mode instead.

 let _ = { (point: CGFloat) in
   let _: SIMD2<Double>? = SIMD2(point, point)
 }

// CHECK-NOT: failed constraint CGFloat transitive conformance to SIMDScalar
// CHECK-NOT: Attempting to salvage
