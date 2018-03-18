func foo1() -> Int { return 0 }

func foo(_ a : Int) -> Int {
  let a1 = 1 + 2 + 2 + 2 + a + foo1()
  let a2 = 1 + 2 + 2 + 2 + a + foo1()
  let a3 = 1 + 2 + 2 + 2 + a + foo1()
  return foo1() + foo1() + foo1() + a1 + a2 + a3
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-repeat -source-filename %s -pos=4:16 -end-pos=4:17 >> %t.result/two.swift
// RUN: diff -u %S/Outputs/basic/two.swift.expected %t.result/two.swift
// RUN: %refactor -extract-repeat -source-filename %s -pos=4:12 -end-pos=4:17 >> %t.result/one-plus-two.swift
// RUN: diff -u %S/Outputs/basic/one-plus-two.swift.expected %t.result/one-plus-two.swift
// RUN: %refactor -extract-repeat -source-filename %s -pos=4:32 -end-pos=4:38 >> %t.result/foo-one.swift
// RUN: diff -u %S/Outputs/basic/foo-one.swift.expected %t.result/foo-one.swift
