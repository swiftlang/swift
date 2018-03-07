func foo() {
  var aa = 3
  aa = aa + 1
  _ = "before \(aa) after"
  struct S {
    lazy var lazyVal: Int = {
      let myVal = 0
      return myVal
     }()
  }
  return 1
}

// RUN: %empty-directory(%t.result)
// RUN: %sourcekitd-test -req=local-rename -pos=2:8 -name new_name %s -- %s > %t.result/local-rename.swift.expected
// RUN: diff -u %S/local-rename.swift.expected %t.result/local-rename.swift.expected
// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=2:8 %s -- %s > %t.result/local-rename-ranges.swift.expected
// RUN: diff -u %S/local-rename-ranges.swift.expected %t.result/local-rename-ranges.swift.expected
// RUN: %sourcekitd-test -req=find-local-rename-ranges -pos=7:11 %s -- %s > %t.result/local-rename-lazy.swift.expected
// RUN: diff -u %S/local-rename-lazy.swift.expected %t.result/local-rename-lazy.swift.expected

// REQUIRES-ANY: OS=macosx, OS=linux-gnu
