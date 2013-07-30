// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -g -o - | FileCheck %s

// Verify that a helper function that is generated on-the-fly does
// not mess up the linetable of the calling function.

// CHECK: store i64{{.*}}getelementptr
// CHECK: store i64{{.*}}getelementptr{{.*}}, !dbg ![[DBG:[0-9]+]]
// CHECK-NEXT: convertFromBuiltinIntegerLiteral{{.*}}(i128 2)
// CHECK-NOT: ![[DBG]] = metadata !{i32 0, i32 0,

class TurnBasedPolicy {

	typealias Rules = (onWin : Int, onHint : Int, onLose : Int, onWrongGuess: Int, numAttempts : Int, numHints : Int, secretRange : (Int, Int))

	constructor (r : Rules) {
		this.rules = r
	}
	var rules : Rules
	var secret : Int
	var attempts : Int
	var hints : Int
	var hintedRange : (Int, Int)
}

var easyPolicy : TurnBasedPolicy.Rules = (
	onWin : +2,
	onHint : -1,
	onLose : -2,
	onWrongGuess : 0,
	numAttempts : 7,
	numHints : 7,
	secretRange : (1,10)
)

