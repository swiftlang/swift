// RUN: %target-swift-frontend -parse-as-library -Xllvm -new-mangling-for-tests -module-name=test -O %s -emit-ir > %t.ll
// RUN: %FileCheck %s < %t.ll
// RUN: %FileCheck -check-prefix=CHECK-DEAD %s < %t.ll

// protocol descriptor for test.Proto
// CHECK-DAG: @_T04test5ProtoMp =

// reflection metadata field descriptors
// CHECK-DAG: @_T04test7StructAVMF =
// CHECK-DAG: @_T04test7StructBVMF =
// CHECK-DAG: @_T04test7StructCVMF =
// CHECK-DAG: @_T04test7StructDVMF =

// value witness tables
// CHECK-DAG: @_T04test7StructAVWV =
// CHECK-DAG: @_T04test7StructBVWV =
// CHECK-DAG: @_T04test7StructCVWV =
// CHECK-DEAD-NOT: @_T04test7StructDVWV

// nominal type descriptors
// CHECK-DAG: @_T04test7StructAVMn =
// CHECK-DAG: @_T04test7StructBVMn =
// CHECK-DAG: @_T04test7StructCVMn =
// CHECK-DEAD-NOT: @_T04test7StructDVMn

// full type metadata
// CHECK-DAG: @_T04test7StructAVMf =
// CHECK-DAG: @_T04test7StructBVMf =
// CHECK-DAG: @_T04test7StructCVMf =
// CHECK-DEAD-NOT: @_T04test7StructDVMf

// protocol witness tables
// CHECK-DAG: @_T04test7StructAVAA5ProtoAAWP =
// CHECK-DAG: @_T04test7StructBVAA5ProtoAAWP =
// CHECK-DAG: @_T04test7StructCVAA5ProtoAAWP =
// CHECK-DEAD-NOT: @_T04test7StructDVAA5ProtoAAWP =

public protocol Proto {
	func abc()
}

struct StructA : Proto {
	func abc() {
	}
}

struct StructB : Proto {
	func abc() {
	}
}

struct StructC : Proto {
	func abc() {
	}
}

// This is the only struct for which no metadata and conformances are needed.
struct StructD : Proto {
	func abc() {
	}
}

@inline(never)
@_semantics("optimize.sil.never")
func consume1<T>(_ t: T) {
}

@inline(never)
@_semantics("optimize.sil.never")
func consume2<T: Proto>(_ t: T) {
	t.abc()
}
@inline(never)
@_semantics("optimize.sil.never")
func consume3(_ p: Proto) {
	p.abc()
}
@inline(never)
@_semantics("optimize.sil.never")
func consume4(_ t: StructD) {
  t.abc()
}

var a = StructA()
var b = StructB()
var c = StructC()
var d = StructD()

public func callfuncA() {
	consume1(a)
}

public func callfuncB() {
	consume2(b)
}

public func callfuncC() {
	consume3(c)
}

public func callfuncD() {
	consume4(d)
}

