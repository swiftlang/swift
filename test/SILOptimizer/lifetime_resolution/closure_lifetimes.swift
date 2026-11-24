// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -sil-verify-all -module-name test \
// RUN:   -sil-stop-optzns-before-lowering-ownership %s | %FileCheck %s --enable-var-scope

// An escaping closure must outlive the uses of its conversion to a non-escaping closure.

// CHECK-LABEL: sil hidden [ossa] @$s4test11countVowelsySiSSF :
// CHECK:     [[CLOSURE:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(thin) (@guaranteed Set<Character>) -> @owned @callee_guaranteed (@guaranteed Character) -> Bool
// CHECK:     convert_escape_to_noescape [[CLOSURE]]
// CHECK-NOT: destroy_value [[CLOSURE]]
// CHECK:     apply [nothrow] {{%.*}}<String, Never>(
// CHECK:     destroy_value [[CLOSURE]]
// CHECK-LABEL: } // end sil function
func countVowels(_ text: String) -> Int {
  let vowels = Set("aeiou")
  return text.count(where: vowels.contains)
}

func getNonThrowingStr(_ i: Int) -> String { "nothrow" }
func getString() throws -> String { "" }
func keep(_ f: @escaping () -> String) {}

// Both stores that initialize the captured variable stay initializations, and the store after the
// closure captures it is an assignment.
// CHECK-LABEL: sil hidden [ossa] @$s4test15initThenEscapedyyKF :
// CHECK:         alloc_box ${ var String }, var, name "x"
// CHECK:         try_apply {{%.*}}() : {{.*}}, normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK:       [[NORMAL]]({{.*}}):
// CHECK:         store {{%.*}} to [init] {{%.*}}
// CHECK:         br [[JOIN:bb[0-9]+]]
// CHECK:       [[JOIN]]:
// CHECK:         partial_apply
// CHECK:         store {{%.*}} to [assign] {{%.*}}
// CHECK:       [[ERROR]]({{.*}}):
// CHECK:         store {{%.*}} to [init] {{%.*}}
// CHECK:         br [[JOIN]]
// CHECK-LABEL: } // end sil function '$s4test15initThenEscapedyyKF'
func initThenEscaped() throws {
  var x: String
  do {
    x = try getString()
  } catch {
    x = getNonThrowingStr(1)
  }
  keep { x }
  x = getNonThrowingStr(2)
}
