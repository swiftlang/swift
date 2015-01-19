// This makes sure -debug-forbid-typecheck-prefix works as expected.

// RUN: not %target-swift-frontend -parse %s -D TRY1 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK1 -input-file %t.txt %s
#if TRY1
// CHECK1: LLVM ERROR: forbidden typecheck occurred: FORBID_global
var FORBID_global = 0
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY2 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK2 -input-file %t.txt %s
#if TRY2
// CHECK2: LLVM ERROR: forbidden typecheck occurred: FORBID_class
class FORBID_class {}
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY3 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK3 -input-file %t.txt %s
#if TRY3
class C {
  // CHECK3: LLVM ERROR: forbidden typecheck occurred: FORBID_member
  var FORBID_member = 0
}
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY4 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK4 -input-file %t.txt %s
#if TRY4
class C {
  // CHECK4: LLVM ERROR: forbidden typecheck occurred: FORBID_memberFunc
  func FORBID_memberFunc() {}
}
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY5 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK5 -input-file %t.txt %s
#if TRY5
// CHECK5: LLVM ERROR: forbidden typecheck occurred: FORBID_func
func FORBID_func() {}
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY6 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK6 -input-file %t.txt %s
#if TRY6
// CHECK6: LLVM ERROR: forbidden typecheck occurred: FORBID_local
func globalFunc() {
	var FORBID_local = 0
}
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY7 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK7 -input-file %t.txt %s
#if TRY7
// CHECK7: LLVM ERROR: forbidden typecheck occurred: FORBID_ref
var global = FORBID_ref
#endif

// RUN: not %target-swift-frontend -parse %s -D TRY8 -debug-forbid-typecheck-prefix FORBID_ 2> %t.txt
// RUN: FileCheck -check-prefix=CHECK8 -input-file %t.txt %s
#if TRY8
class C {
  // CHECK8: LLVM ERROR: forbidden typecheck occurred: FORBID_ref
  var member = FORBID_ref
}
#endif
