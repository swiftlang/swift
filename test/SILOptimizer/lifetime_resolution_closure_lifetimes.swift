// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -module-name test %s | %FileCheck %s

// An escaping closure must outlive the uses of its conversion to a non-escaping closure.

// CHECK-LABEL: sil hidden @$s4test11countVowelsySiSSF :
// CHECK:     [[CLOSURE:%.*]] = apply {{%.*}}({{%.*}}) : $@convention(thin) (@guaranteed Set<Character>) -> @owned @callee_guaranteed (@guaranteed Character) -> Bool
// CHECK:     convert_escape_to_noescape [[CLOSURE]]
// CHECK-NOT: strong_release [[CLOSURE]]
// CHECK:     apply [nothrow] {{%.*}}<String, Never>(
// CHECK:     strong_release [[CLOSURE]]
// CHECK-LABEL: } // end sil function
func countVowels(_ text: String) -> Int {
  let vowels = Set("aeiou")
  return text.count(where: vowels.contains)
}
