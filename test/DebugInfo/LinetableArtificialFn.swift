// RUN: %target-swift-frontend %s -emit-ir -g -o - -disable-sil-linking | FileCheck %s

// Verify that a helper function that is generated on-the-fly does
// not mess up the linetable of the calling function.

// CHECK: store {{(i32|i64)}} {{.*}}getelementptr
// CHECK: store {{(i32|i64)}} {{.*}}getelementptr{{.*}}, !dbg ![[DBG:[0-9]+]]
// CHECK-NEXT: _TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si{{.*}}(i2048 -2)
// CHECK-NOT: ![[DBG]] = !{i32 0, i32 0,

class TurnBasedPolicy {

	typealias Rules = (onWin : Int, onHint : Int, onLose : Int, onWrongGuess: Int, numAttempts : Int, numHints : Int, secretRange : (Int, Int))

	init (r : Rules) {
		self.rules = r
	}
	var rules : Rules
	var secret = 0
	var attempts = 0
	var hints = 0
	var hintedRange = (0, 0)
}

var easyPolicy : TurnBasedPolicy.Rules = (
	onWin : 2,
	onHint : -1,
	onLose : -2,
	onWrongGuess : 0,
	numAttempts : 7,
	numHints : 7,
	secretRange : (1,10)
)

