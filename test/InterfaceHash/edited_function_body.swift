// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/ModuleCache)
// RUN: %{python} %utils/split_file.py -o %t %s
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/a.swift -module-cache-path %t/ModuleCache 2> %t/a.hash
// RUN: %target-swift-frontend -dump-interface-hash -primary-file %t/b.swift -module-cache-path %t/ModuleCache 2> %t/b.hash
// RUN: cmp %t/a.hash %t/b.hash

// BEGIN a.swift
func f() -> Int {
    return 0
}

// BEGIN b.swift
func f() -> Int {
    return 1
}
