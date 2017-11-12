// RUN: %target-swift-frontend %s -emit-ir

class Expression<A, R> {
	typealias Arg = A
	typealias Ret = R
	subscript(x: Arg) -> Ret! { return nil }
}
class Op<A, R> : Expression<A, R> {
	typealias OpType = (Arg) -> Ret
	let op: OpType
	init(op: @escaping OpType) {
		self.op = op
		super.init()
	}
}

class BinaryOp<A1, A2, R> : Op<((A1, A2)), R> {
	override init(op: @escaping OpType) {
		super.init(op: op)
	}
	override subscript(x: Arg) -> Ret! {
		return op(x)
	}
}
let add = BinaryOp<Int, Int, Int> { return $0.0 + $0.1 }
print(add[(1,1)])
