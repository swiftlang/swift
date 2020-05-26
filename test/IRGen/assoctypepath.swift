// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s

// Check that we can mangle+demangle the associated type witness table accessor
// and do not crash

protocol V {}
protocol W: V {}
protocol X: W {}
protocol Y: X {}
protocol Z: W {
	associatedtype ZZ: X
}

protocol P {
	associatedtype A: W
}
protocol Q: P where A: Z {}
protocol R: Q where A.ZZ: Y {}

// CHECK: define {{.*}} @"$s13assoctypepath1SVyxGAA1RAA1A_2ZZAA1YPWT"

struct S<T>: R where T: Z, T.ZZ: Y {
	typealias A = T
}
