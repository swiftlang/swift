// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/a.swift 2> %t/a.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/b.swift 2> %t/b.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/a.swift -experimental-skip-all-function-bodies 2> %t/c.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/b.swift -experimental-skip-all-function-bodies 2> %t/d.hash
// RUN: cmp %t/a.hash %t/b.hash
// RUN: cmp %t/a.hash %t/c.hash
// RUN: cmp %t/a.hash %t/d.hash

// Make sure "interface hash" doesn't change after modifying type members, and 

// BEGIN a.swift
class C {
  func foo() {}
}

// BEGIN b.swift
class C {
  func foo() {}
  func bar() {}
}
