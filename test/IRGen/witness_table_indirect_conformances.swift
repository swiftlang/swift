// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -primary-file %s -emit-ir -disable-objc-attr-requires-foundation-module -swift-version 4 | %FileCheck %s -DINT=i%target-ptrsize

protocol P1 {
	associatedtype AssocP1
}

protocol P2 {
	associatedtype AssocP2: P1

	func getAssocP2() -> AssocP2
}

protocol P3 {
	associatedtype AssocP3: P2 where AssocP3.AssocP2: Q

	func getAssocP3() -> AssocP3
}

protocol Q { }

struct X { }

struct Y: P1, Q {
	typealias AssocP1 = X
}

struct Z: P2 {
	typealias AssocP2 = Y

	func getAssocP2() -> Y { return Y() }
}

// CHECK: @"$S35witness_table_indirect_conformances1WVAA2P3AAWP" = hidden constant [5 x i8*] [
// CHECK-SAME: @"$S35witness_table_indirect_conformances1WVAA2P3AAMc"
// CHECK-SAME: i8* bitcast (%swift.metadata_response ([[INT]])* @"$S35witness_table_indirect_conformances1ZVMa" to i8*), i8* bitcast (i8** ()* @"$S35witness_table_indirect_conformances1ZVAA2P2AAWa" to i8*),
// CHECK-SAME: i8* bitcast (i8** ()* @"$S35witness_table_indirect_conformances1YVAA1QAAWa" to i8*),
// CHECK-SAME: i8* bitcast (void (%T35witness_table_indirect_conformances1ZV*, %T35witness_table_indirect_conformances1WV*, %swift.type*, i8**)* @"$S35witness_table_indirect_conformances1WVAA2P3A2aDP08getAssocE00gE0QzyFTW" to i8*)]
struct W: P3 {
	typealias AssocP3 = Z

	func getAssocP3() -> Z { return Z() }
}

// CHECK-LABEL: define hidden i8** @"$S35witness_table_indirect_conformances1YVAA1QAAWa"()
// CHECK-NEXT: entry:
// CHECK-NEXT: ret i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$S35witness_table_indirect_conformances1YVAA1QAAWP", i32 0, i32 0)

// CHECK-LABEL: define hidden i8** @"$S35witness_table_indirect_conformances1ZVAA2P2AAWa"()
// CHECK-NEXT: entry:
// CHECK: ret i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @"$S35witness_table_indirect_conformances1ZVAA2P2AAWP", i32 0, i32 0)

// CHECK-LABEL: define hidden swiftcc %swift.metadata_response @"$S35witness_table_indirect_conformances1ZVMa"
// CHECK-SAME:    ([[INT]])
// CHECK-NEXT: entry:
// CHECK-NEXT: ret %swift.metadata_response { %swift.type* bitcast {{.*}} @"$S35witness_table_indirect_conformances1ZVMf", i32 0, i32 1) to %swift.type*), [[INT]] 0 }
