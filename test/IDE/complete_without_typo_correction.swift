// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=HERE -code-completion-diagnostics 2> %t.err.txt
// RUN: %FileCheck %s -input-file=%t.err.txt

// CHECK: use of unresolved identifier 'foo11'
// CHECK-NOT: did you mean

func foo12() -> Int { return 0 }

class C {
  var p = foo11()
}

C().#^HERE^#
