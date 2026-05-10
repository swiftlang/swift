// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -primary-file %t/a.swift %t/b.swift -plugin-path %swift-plugin-dir

//--- a.swift

func foo() {
  _ = {
    let i = 0
    $bar.withValue(i) {}
  }
}

//--- b.swift

@TaskLocal
var bar: Int?
