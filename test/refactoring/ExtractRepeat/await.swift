func myAsync() async -> Int { 0 }
func myFoo() -> Int { 0 }

func testExtract() async -> Int {
  let a = myFoo()
  let b = myFoo()
  let x = await myAsync()
  let y = await myAsync()
  return a + b + x + y
}

// rdar://72199992
// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-repeat -source-filename %s -pos=5:11 -end-pos=5:18 >> %t.result/one.swift
// RUN: diff -u %S/Outputs/await/one.swift.expected %t.result/one.swift
// RUN: %refactor -extract-repeat -source-filename %s -pos=7:11 -end-pos=7:26 >> %t.result/two.swift
// RUN: diff -u %S/Outputs/await/two.swift.expected %t.result/two.swift
