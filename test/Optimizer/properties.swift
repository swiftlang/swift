// RUN: %swift -O3 %s | FileCheck %s
// XFAIL: *

func storeTest(arg : Int)
func loadTest() -> Int
var test : Int {
  get {
    return loadTest()
  } set {
   storeTest(value)
  }
}

// Properties shouldn't have side effects, therefore the compiler can/should
// optimize away redundant loads and stores

test = 123
test = 456
test = 789
println(test)

// the above should generate one store, and no load:
//
// CHECK: define i32 @main() {
// CHECK-NEXT: entry:
// CHECK-NEXT: tail call void @_T10properties9storeTestFT3argSi_T_(i64 789)
// CHECK-NEXT: tail call void @_TSs5printFT3valSi_T_(i64 789)

var tmp = test
tmp = test
tmp = test
println(tmp)

// only the last load above should survive
//
// CHECK: %0 = tail call i64 @_T10properties8loadTestFT_Si()
// CHECK-NEXT: store i64 %0, i64* getelementptr inbounds (%_TSs5Int64* @_T10properties3tmpSi, i64 0, i32 0), align 8
// CHECK-NEXT: tail call void @_TSs5printFT3valSi_T_(i64 %0)
