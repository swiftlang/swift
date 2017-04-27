func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa == 3 { aaa = 4 }
  return aaa
}

func foo1() -> Int { return 0 }
class C { func foo() {} }
struct S { func foo() {} }

// RUN: %sourcekitd-test -req=range -pos=2:13 -length 5 %s -- %s | %FileCheck %s -check-prefix=CHECK1

// RUN: %sourcekitd-test -req=range -pos=3:3 -length 13 %s -- %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %sourcekitd-test -req=range -pos=3:1 -length 15 %s -- %s | %FileCheck %s -check-prefix=CHECK2

// RUN: %sourcekitd-test -req=range -pos=4:1 -length 25 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=range -pos=4:1 -length 26 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=range -pos=4:1 -length 27 %s -- %s | %FileCheck %s -check-prefix=CHECK3

// RUN: %sourcekitd-test -req=range -pos=5:1 -length 12 %s -- %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %sourcekitd-test -req=range -pos=5:2 -length 11 %s -- %s | %FileCheck %s -check-prefix=CHECK4

// RUN: %sourcekitd-test -req=range -pos=8:1 -length 31 %s -- %s | %FileCheck %s -check-prefix=CHECK5
// RUN: %sourcekitd-test -req=range -pos=9:1 -length 25 %s -- %s | %FileCheck %s -check-prefix=CHECK6
// RUN: %sourcekitd-test -req=range -pos=10:1 -length 26 %s -- %s | %FileCheck %s -check-prefix=CHECK7
// RUN: %sourcekitd-test -req=range -pos=3:1 -length 42 %s -- %s | %FileCheck %s -check-prefix=CHECK8
// RUN: %sourcekitd-test -req=range -pos=3:1 -length 55 %s -- %s | %FileCheck %s -check-prefix=CHECK9
// RUN: %sourcekitd-test -req=range -pos=4:1 -length 36 %s -- %s | %FileCheck %s -check-prefix=CHECK10

// RUN: %sourcekitd-test -req=range -pos=8:1 -end-pos 8:32 %s -- %s | %FileCheck %s -check-prefix=CHECK5
// RUN: %sourcekitd-test -req=range -pos=9:1 -end-pos 9:26 %s -- %s | %FileCheck %s -check-prefix=CHECK6
// RUN: %sourcekitd-test -req=range -pos=10:1 -end-pos 10:27 %s -- %s | %FileCheck %s -check-prefix=CHECK7
// RUN: %sourcekitd-test -req=range -pos=3:1 -end-pos=4:26 %s -- %s | %FileCheck %s -check-prefix=CHECK8
// RUN: %sourcekitd-test -req=range -pos=3:1 -end-pos=5:13 %s -- %s | %FileCheck %s -check-prefix=CHECK9
// RUN: %sourcekitd-test -req=range -pos=4:1 -end-pos=5:13 %s -- %s | %FileCheck %s -check-prefix=CHECK10

// CHECK1-DAG: <kind>source.lang.swift.range.singleexpression</kind>
// CHECK1-DAG: <content>1 + 2</content>
// CHECK1-DAG: <type>Int</type>

// CHECK2-DAG: <kind>source.lang.swift.range.singleexpression</kind>
// CHECK2-DAG: <content>aaa = aaa + 3</content>
// CHECK2-DAG: <type>()</type>

// CHECK3-DAG: <kind>source.lang.swift.range.singlestatement</kind>
// CHECK3-DAG: <content>if aaa == 3 { aaa = 4 }</content>
// CHECK3-DAG: <type></type>

// CHECK4-DAG: <kind>source.lang.swift.range.singlestatement</kind>
// CHECK4-DAG: <content>return aaa</content>
// CHECK4-DAG: <type></type>

// CHECK5-DAG: <kind>source.lang.swift.range.singledeclaration</kind>
// CHECK5-DAG: <content>func foo1() -> Int { return 0 }</content>
// CHECK5-DAG: <type></type>

// CHECK6-DAG: <kind>source.lang.swift.range.singledeclaration</kind>
// CHECK6-DAG: <content>class C { func foo() {} }</content>
// CHECK6-DAG: <type></type>

// CHECK7-DAG: <kind>source.lang.swift.range.singledeclaration</kind>
// CHECK7-DAG: <content>struct S { func foo() {} }</content>
// CHECK7-DAG: <type></type>

// CHECK8-DAG: <kind>source.lang.swift.range.multistatement</kind>
// CHECK8-DAG: <content>aaa = aaa + 3
// CHECK8-DAG:   if aaa == 3 { aaa = 4 }</content>
// CHECK8-DAG: <type></type>

// CHECK9-DAG: <kind>source.lang.swift.range.multistatement</kind>
// CHECK9-DAG: <content>aaa = aaa + 3
// CHECK9-DAG:   if aaa == 3 { aaa = 4 }
// CHECK9-DAG:   return aaa</content>
// CHECK9-DAG: <type></type>

// CHECK10-DAG: <kind>source.lang.swift.range.multistatement</kind>
// CHECK10-DAG: <content>if aaa == 3 { aaa = 4 }
// CHECK10-DAG:   return aaa</content>
// CHECK10-DAG: <type></type>
