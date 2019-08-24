// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/a.swift 2> %t/a.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/b.swift 2> %t/b.hash
// RUN: not cmp %t/a.hash %t/b.hash

// BEGIN a.swift
private struct S {
  func f2() -> Int {
    return 0
  }

  var y: Int = 0
}

// BEGIN b.swift
private struct S {
  func f2() -> Int {
    return 0
  }

  var x: Int = 0
  var y: Int = 0
}
