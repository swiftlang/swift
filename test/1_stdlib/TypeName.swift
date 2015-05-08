// RUN: %target-run-simple-swift | FileCheck %s

// XFAIL: linux

class C {}
struct S {}
enum E {}

protocol P {}
protocol P2 {}
protocol AssociatedTypes {
  typealias A
  typealias B
  typealias C
}

class Model : AssociatedTypes {
  typealias A = C
  typealias B = S
  typealias C = E
}

struct Model2 : AssociatedTypes {
  typealias A = C
  typealias B = S
  typealias C = E
}

class GC<T : AssociatedTypes> {}
struct GS<T : AssociatedTypes> {}
enum GE<T : AssociatedTypes> {}
class GC2<T : AssociatedTypes, U : AssociatedTypes> {}

func printTypeName(t: Any.Type) { print(_typeName(t)) }

printTypeName(Int.self) // CHECK: Swift.Int
printTypeName(C.self) // CHECK-NEXT: [[THIS:.*]].C
printTypeName(S.self) // CHECK-NEXT: [[THIS]].S
printTypeName(E.self) // CHECK-NEXT: [[THIS]].E
printTypeName(GC<Model>.self) // CHECK-NEXT: [[THIS]].GC<[[THIS]].Model>
printTypeName(GS<Model>.self) // CHECK-NEXT: [[THIS]].GS<[[THIS]].Model>
printTypeName(GE<Model>.self) // CHECK-NEXT: [[THIS]].GE<[[THIS]].Model>
printTypeName(GC2<Model, Model2>.self) // CHECK-NEXT: [[THIS]].GC2<[[THIS]].Model, [[THIS]].Model2>

printTypeName(P.self) // CHECK-NEXT: [[THIS]].P
typealias PP2 = protocol<P, P2>
printTypeName(PP2.self) // CHECK-NEXT: protocol<[[THIS]].P, [[THIS]].P2>
printTypeName(Any.self) // CHECK-NEXT: protocol<>

typealias F = () -> ()
typealias F2 = () -> () -> ()
typealias F3 = (() -> ()) -> ()

printTypeName(F.self) // CHECK-NEXT: () -> ()
printTypeName(F2.self) // CHECK-NEXT: () -> () -> ()
printTypeName(F3.self) // CHECK-NEXT: (() -> ()) -> ()

typealias B = @convention(block) () -> ()
typealias B2 = () -> @convention(block) () -> ()
typealias B3 = (@convention(block) () -> ()) -> ()

printTypeName(B.self) // CHECK-NEXT: @convention(block) () -> ()
printTypeName(B2.self) // CHECK-NEXT: () -> @convention(block) () -> ()
printTypeName(B3.self) // CHECK-NEXT: (@convention(block) () -> ()) -> ()

printTypeName(F.Type.self) // CHECK-NEXT: (() -> ()).Type
printTypeName(C.Type.self) // CHECK-NEXT: [[THIS]].C.Type
printTypeName(C.Type.Type.self) // CHECK-NEXT: [[THIS]].C.Type.Type
printTypeName(Any.Type.self) // CHECK-NEXT: protocol<>.Type
printTypeName(Any.Protocol.self) // CHECK-NEXT: protocol<>.Protocol
printTypeName(AnyObject.self) // CHECK-NEXT: {{^}}Swift.AnyObject{{$}}
printTypeName(AnyClass.self) // CHECK-NEXT: {{^}}Swift.AnyObject.Type{{$}}
printTypeName((AnyObject?).self) // CHECK-NEXT: {{^}}Swift.Optional<Swift.AnyObject>{{$}}

printTypeName(Void.self) // CHECK-NEXT: ()
typealias Tup = (Any, F, C)
printTypeName(Tup.self) // CHECK-NEXT: (protocol<>, () -> (), [[THIS]].C)

typealias IF = inout Int -> ()
typealias IF2 = inout Int -> inout Int -> ()
typealias IF3 = (inout Int -> ()) -> ()
typealias IF4 = inout (() -> ()) -> ()
typealias IF5 = (inout Int, Any) -> ()

printTypeName(IF.self) // CHECK-NEXT: inout Swift.Int -> ()
printTypeName(IF2.self) // CHECK-NEXT: inout Swift.Int -> inout Swift.Int -> ()

// FIXME: this is wrong.  Should be: "(inout Swift.Int -> ()) -> ()"
// <rdar://problem/20133773> Demangling of function types that include 'inout' is wrong
printTypeName(IF3.self) // CHECK-NEXT: inout (Swift.Int -> ()) -> ()

// FIXME: this is wrong.  Should be: "inout (() -> ()) -> ()"
// <rdar://problem/20133773> Demangling of function types that include 'inout' is wrong
printTypeName(IF4.self) // CHECK-NEXT: (() -> ()) -> ()

printTypeName(IF5.self) // CHECK-NEXT: (inout Swift.Int, protocol<>) -> ()

func curry1() {

}

func curry1Throws() throws {

}

func curry2() -> () -> () {
	return curry1
}

func curry2Throws() throws -> () -> () {
	return curry1
}

func curry3() -> () throws -> () {
	return curry1Throws
}

func curry3Throws() throws -> () throws -> () {
	return curry1Throws
}

printTypeName(curry1.dynamicType) // CHECK-NEXT: () -> ()

printTypeName(curry2.dynamicType) // CHECK-NEXT: () -> () -> ()

printTypeName(curry2Throws.dynamicType) // CHECK-NEXT: () throws -> () -> ()

printTypeName(curry3.dynamicType) // CHECK-NEXT: () -> () throws -> ()

printTypeName(curry3Throws.dynamicType) // CHECK-NEXT: () throws -> () throws -> ()
