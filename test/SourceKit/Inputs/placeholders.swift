class <#MyCls#> : <#OtherClass#> {}

func <#test1#> () {
  for <#name#> in <#items#> {}
}

// CHECK: <gvar>let <name>myArray</name> = <array>[<elem-expr><#item1#></elem-expr>, <elem-expr><#item2#></elem-expr>]</array></gvar>
let myArray = [<#item1#>, <#item2#>]
