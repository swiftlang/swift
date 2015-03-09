// RUN: %swift-ide-test -structure -source-filename %s 2> %t.err.txt | FileCheck %s

// CHECK: <class>class <name><#MyCls#></name> : <inherited><elem-typeref><#OtherClass#></elem-typeref></inherited> {}
class <#MyCls#> : <#OtherClass#> {}

func <#test1#> () {
  for <#name#> in <#items#> {}
}

// CHECK: <gvar>let <name>myArray</name> = <array>[<elem-expr><#item1#></elem-expr>, <elem-expr><#item2#></elem-expr>]</array></gvar>
let myArray = [<#item1#>, <#item2#>]

// This is to make sure that the error diagnostic is in the right location and that there is no other error.
// RUN: FileCheck -input-file %t.err.txt %s -check-prefix=CHECK-ERR
// RUN: cat %t.err.txt | grep 'error:' | count 1
// CHECK-ERR: [[@LINE+1]]:1: error: missing initialization in a 'for' statement
for {}
