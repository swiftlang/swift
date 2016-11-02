func foo() -> Int{
  var aaa = 1 + 2
  aaa = aaa + 3
  if aaa = 3 { aaa = 4 }
  return aaa
}

// RUN: %sourcekitd-test -req=range -pos=2:13 -length 5 %s -- %s | %FileCheck %s -check-prefix=CHECK1

// RUN: %sourcekitd-test -req=range -pos=3:3 -length 13 %s -- %s | %FileCheck %s -check-prefix=CHECK2
// RUN: %sourcekitd-test -req=range -pos=3:1 -length 15 %s -- %s | %FileCheck %s -check-prefix=CHECK2

// RUN: %sourcekitd-test -req=range -pos=4:1 -length 24 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=range -pos=4:1 -length 25 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=range -pos=4:1 -length 26 %s -- %s | %FileCheck %s -check-prefix=CHECK3
// RUN: %sourcekitd-test -req=range -pos=4:4 -length 21 %s -- %s | %FileCheck %s -check-prefix=CHECK3

// RUN: %sourcekitd-test -req=range -pos=5:1 -length 12 %s -- %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %sourcekitd-test -req=range -pos=5:2 -length 11 %s -- %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %sourcekitd-test -req=range -pos=5:5 -length 8 %s -- %s | %FileCheck %s -check-prefix=CHECK4
// RUN: %sourcekitd-test -req=range -pos=5:5 -length 9 %s -- %s | %FileCheck %s -check-prefix=CHECK4

// CHECK1-DAG: <kind>source.lang.swift.range.expression</kind>
// CHECK1-DAG: <content>1 + 2</content>
// CHECK1-DAG: <type>Int</type>

// CHECK2-DAG: <kind>source.lang.swift.range.expression</kind>
// CHECK2-DAG: <content>aaa = aaa + 3</content>
// CHECK2-DAG: <type>()</type>

// CHECK3-DAG: <kind>source.lang.swift.range.singlestatement</kind>
// CHECK3-DAG: <content>if aaa = 3 { aaa = 4 }</content>
// CHECK3-DAG: <type></type>

// CHECK4-DAG: <kind>source.lang.swift.range.singlestatement</kind>
// CHECK4-DAG: <content>return aaa</content>
// CHECK4-DAG: <type></type>
