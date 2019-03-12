// RUN: %target-swift-ide-test -code-completion -code-completion-token=COMPLETE -source-filename=%s | %FileCheck %s

// REQUIRES: long_test

// This used to take ~6 min to complete.
// Now it's 2 min: rdar://problem/48818341

func testing() {
  return (["a"] + [1].map { String($0) })
    .map { $0 + "b" as String }
    .filter { $0 != "" } #^COMPLETE^#
}
// CHECK: Decl[InfixOperatorFunction]/{{.*}}: [' ']+ {#[String]#}[#[String]#]; name=+ [String]
