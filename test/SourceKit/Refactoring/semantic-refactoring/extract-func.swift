func foo() -> Int {
  var a = 3
  a = a + 1
  return 1
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=extract-func -pos=2:1 -end-pos 4:11 -name new_name %s -- %s > %t.result/extract-func.swift.expected
// RUN: diff -u %S/extract-func.swift.expected %t.result/extract-func.swift.expected

// FIXME: Fails on linux with assertion: "!GlibcModuleMapPath.empty()"" failed
// REQUIRES-ANY: OS=macosx
