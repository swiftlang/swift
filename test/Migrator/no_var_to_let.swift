// RUN: %target-swift-frontend -typecheck %s -swift-version 3
// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -update-code -primary-file %s -emit-migrated-file-path %t/no_var_to_let.swift.result -swift-version 3 -o /dev/null
// RUN: diff -u %s %t/no_var_to_let.swift.result
// RUN: %target-swift-frontend -typecheck %s -swift-version 4

// Note that the diff run line indicates that there should be no change.

// The compiler should not suggest `let` instead of `var` here because
// it is a compile error to say `for let ...`.
// rdar://problem/32390726

for var i in 0..<10 {
  _ = i + 1
}
