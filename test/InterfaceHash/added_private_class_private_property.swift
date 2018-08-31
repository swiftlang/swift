// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -dump-interface-hash %t/a.swift 2> %t/a.hash
// RUN: %target-swift-frontend -dump-interface-hash %t/b.swift 2> %t/b.hash
// RUN: not cmp %t/a.hash %t/b.hash

// BEGIN a.swift
private class C {
  func f2() -> Int {
    return 0
  }
}

// BEGIN b.swift
private class C {
  func f2() -> Int {
    return 0
  }

  private var x: Int = 0
}
