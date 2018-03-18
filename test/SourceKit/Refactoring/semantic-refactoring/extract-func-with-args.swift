func foo() -> Int {
  var a = 3
  a = a + 1
  return 1
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=extract-func -pos=3:1 -end-pos 3:12 -name new_name %s -- %s > %t.result/extract-func-with-args.swift.expected
// RUN: diff -u %S/extract-func-with-args.swift.expected %t.result/extract-func-with-args.swift.expected

// FIXME: Fails on linux with assertion: "!GlibcModuleMapPath.empty()"" failed
// REQUIRES-ANY: OS=macosx
