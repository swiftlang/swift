func test() {
  func1()
    { 1 } 
}

func func1(
  fn1: () -> Int,
  fn2: () -> Void = {},
  fn3: (Int) -> Void = {_ in},
  fn4: (Int, String) -> Void = {_,_ in},
  fn5: (Int, String) -> Int = {_,_ in 1},
  fn7: (inout Int) -> Void = {_ in},
  fn8: (Int...) -> Void = { (_:Int...) in})
{}

// RUN: %sourcekitd-test -req=complete -pos=3:11 %s -- %s > %t
// RUN: %FileCheck %s < %t
// RUN: %FileCheck %s --check-prefix=DESCRIPTION < %t

// CHECK: key.results: [
// CHECK-DAG: key.sourcetext: "fn2: {\n<#code#>\n}"
// CHECK-DAG: key.sourcetext: "fn3: { <#Int#> in\n<#code#>\n}"
// CHECK-DAG: key.sourcetext: "fn4: { <#Int#>, <#String#> in\n<#code#>\n}"
// CHECK-DAG: key.sourcetext: "fn5: { <#Int#>, <#String#> in\n<#code#>\n}"
// CHECK-DAG: key.sourcetext: "fn7: { <#inout Int#> in\n<#code#>\n}"
// CHECK-DAG: key.sourcetext: "fn8: { <#Int...#> in\n<#code#>\n}"
// CHECK: ]

// DESCRIPTION-NOT: key.description: "fn{{[0-9]*}}: {
