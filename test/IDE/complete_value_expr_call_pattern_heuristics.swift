// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BEFORE_COMMA -code-complete-call-pattern-heuristics | %FileCheck %s -check-prefix=BEFORE_COMMA
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=BEFORE_PLACEHOLDER -code-complete-call-pattern-heuristics | %FileCheck %s -check-prefix=BEFORE_PLACEHOLDER

// FIXME: This tests are extracted from complete_value_expr.swift because
// '-batch-code-completion' doesn't support '-code-complete-call-pattern-heuristics'
// Please moved them back to it when it's supported.

struct FooStruct {
  mutating func instanceFunc2(_ a: Int, b: inout Double) {}
}

func testInsideFunctionCall_1(_ x: inout FooStruct) {
  x.instanceFunc(#^BEFORE_COMMA^#,
// BEFORE_COMMA: Begin completions
// BEFORE_COMMA-NOT: Pattern/{{.*}}:{{.*}}({{.*}}{#Int#}
// BOFORE_COMMA-NOT: Decl[InstanceMethod]/{{.*}}:{{.*}}({{.*}}{#Int#}
// BEFORE_COMMA: End completions
}
func testInsideFunctionCall_2(_ x: inout FooStruct) {
  x.instanceFunc(#^BEFORE_PLACEHOLDER^#<#placeholder#>
// BEFORE_PLACEHOLDER: Begin completions
// BEFORE_PLACEHOLDER-NOT: Pattern/{{.*}}:{{.*}}({{.*}}{#Int#}
// BOFORE_PLACEHOLDER-NOT: Decl[InstanceMethod]/{{.*}}:{{.*}}({{.*}}{#Int#}
// BEFORE_PLACEHOLDER: End completions
}
