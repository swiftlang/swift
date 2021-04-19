// RUN: %empty-directory(%t)
// RUN: %target-swift-ide-test -batch-code-completion -source-filename %s -filecheck %raw-FileCheck -completion-output-dir %t -code-complete-call-pattern-heuristics

struct FooStruct {
  init() {}
  init(a: Int) {}
  init(a: Int, b: Float) {}
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
func testConstructor() {
  FooStruct(#^CONSTRUCTOR^#,
// CONSTRUCTOR: Begin completions
// CONSTRUCTOR-NOT: Pattern/{{.*}}
// CONSTRUCTOR-NOT: Decl[Constructor]
// CONSTRUCTOR: Pattern/ExprSpecific: {#a: Int#}[#Int#]
// CONSTRUCTOR-NOT: Pattern/{{.*}}
// CONSTRUCTOR-NOT: Decl[Constructor]
// CONSTRUCTOR: End completions
}

func firstArg(arg1 arg1: Int, arg2: Int) {}
func testArg2Name3() {
  firstArg(#^LABELED_FIRSTARG^#,
// LABELED_FIRSTARG: Begin completions
// LABELED_FIRSTARG-NOT: ['(']{#arg1: Int#}, {#arg2: Int#}[')'][#Void#];
// LABELED_FIRSTARG-DAG: Pattern/ExprSpecific: {#arg1: Int#}[#Int#];
// LABELED_FIRSTARG-NOT: ['(']{#arg1: Int#}, {#arg2: Int#}[')'][#Void#];
// LABELED_FIRSTARG: End completions

