// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=GLOBALFUNC_SAMELINE | %FileCheck %s -check-prefix=GLOBALFUNC_SAMELINE

func func1(
  fn1: () -> Int,
  fn2: () -> Void = {},
  fn3: (Int) -> Void = {_ in},
  fn4: (Int, String) -> Void = {_,_ in},
  fn5: (Int, String) -> Int = {_,_ in 1},
  fn6: (_ a: Int, _ b: String) -> Int = {_,_ in 1},
  fn7: (inout Int) -> Void = {_ in},
  fn8: (Int...) -> Void = { (_:Int...) in})
{}

func test() {
  func1()
    { 1 } #^GLOBALFUNC_SAMELINE^#

// GLOBALFUNC_SAMELINE: Begin completions
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn2:  () -> Void {|}#}[#() -> Void#];
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn3:  (Int) -> Void {<#Int#> in|}#}[#(Int) -> Void#];
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn4:  (Int, String) -> Void {<#Int#>, <#String#> in|}#}[#(Int, String) -> Void#];
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn5:  (Int, String) -> Int {<#Int#>, <#String#> in|}#}[#(Int, String) -> Int#];
// FIXME: recover names
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn6:  (Int, String) -> Int {<#Int#>, <#String#> in|}#}[#(Int, String) -> Int#];
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn7:  (inout Int) -> Void {<#inout Int#> in|}#}[#(inout Int) -> Void#];
// GLOBALFUNC_SAMELINE-DAG: Pattern/ExprSpecific: {#fn8:  (Int...) -> Void {<#Int...#> in|}#}[#(Int...) -> Void#];
// GLOBALFUNC_SAMELINE: End completions
}
