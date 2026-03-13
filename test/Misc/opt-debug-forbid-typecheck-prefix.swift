// This makes sure -debug-forbid-typecheck-prefix works as expected.

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY1 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK1 -input-file %t.txt %s
#if TRY1
// CHECK1: note: forbidden typecheck occurred: FORBID_global
var FORBID_global = 0
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY2 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK2 -input-file %t.txt %s
#if TRY2
// CHECK2: note: forbidden typecheck occurred: FORBID_class
class FORBID_class {}
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY3 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK3 -input-file %t.txt %s
#if TRY3
class C {
  // CHECK3: note: forbidden typecheck occurred: FORBID_member
  var FORBID_member = 0
}
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY4 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK4 -input-file %t.txt %s
#if TRY4
class C {
  // CHECK4: note: forbidden typecheck occurred: FORBID_memberFunc
  func FORBID_memberFunc() {}
}
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY5 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK5 -input-file %t.txt %s
#if TRY5
// CHECK5: note: forbidden typecheck occurred: FORBID_func
func FORBID_func() {}
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY6 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK6 -input-file %t.txt %s
#if TRY6
// CHECK6: note: forbidden typecheck occurred: FORBID_local
func globalFunc() {
	var FORBID_local = 0
}
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY7 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK7 -input-file %t.txt %s
#if TRY7
// CHECK7: note: forbidden typecheck occurred: FORBID_ref
var global = FORBID_ref
#endif

// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY8 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK8 -input-file %t.txt %s
#if TRY8
class C {
  // CHECK8: note: forbidden typecheck occurred: FORBID_ref
  var member = FORBID_ref
}
#endif

// Verify that multiple -debug-forbid-typecheck-prefix arguments may be specified.
// RUN: not --crash %target-swift-frontend -typecheck %s -D TRY9 -debug-forbid-typecheck-prefix FORBID_ -debug-forbid-typecheck-prefix FORBID2_ 2> %t.txt
// RUN: %FileCheck -check-prefix=CHECK9 -input-file %t.txt %s
#if TRY9
// CHECK9: note: forbidden typecheck occurred: FORBID2_global
var FORBID2_global = 0
#endif
