func foo1() -> Int { return 0 }

func foo(_ a : Int) -> Int {
  let new_name = 1
  let new_name1 = 1
  let new_name2 = 1
  let new_name3 = 1
  var aaa = 1 + 2 + 2 + a + foo1()
  return 1
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-expr -source-filename %s -pos=8:13 -end-pos=8:22 >> %t.result/C13-22.swift
// RUN: diff -u %S/Outputs/name_collision/C13-22.swift.expected %t.result/C13-22.swift
