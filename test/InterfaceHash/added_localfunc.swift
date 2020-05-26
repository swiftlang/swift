// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/a.swift 2> %t/a.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/b.swift 2> %t/b.hash
// RUN: cmp %t/a.hash %t/b.hash

// BEGIN a.swift
func test() -> Int {
  return 0
}

// BEGIN b.swift
func test() -> Int {
  func inner() -> Int{
    return 0
  }
  return inner()
}
