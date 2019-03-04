func foo2() -> Int {
  func foo3() -> Int {
    var a = 3
    a = a + 1
    return 1
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=extract-func -pos=3:1 -end-pos 5:13 -name new_name %s -- %s > %t.result/extract-func-idented.swift.expected
// RUN: diff -u %S/extract-func-idented.swift.expected %t.result/extract-func-idented.swift.expected

// FIXME: Fails on linux with assertion: "!GlibcModuleMapPath.empty()"" failed
// REQUIRES-ANY: OS=macosx
