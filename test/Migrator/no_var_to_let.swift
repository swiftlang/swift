// RUN: %target-swift-frontend -typecheck %s -swift-version 4 %api_diff_data_dir
// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %s -emit-migrated-file-path %t/no_var_to_let.swift.result -swift-version 4 -o /dev/null %api_diff_data_dir
// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %s -emit-migrated-file-path %t/no_var_to_let.swift.result -swift-version 4 -o /dev/null %api_diff_data_dir
// RUN: %diff -u %s %t/no_var_to_let.swift.result
// RUN: %target-swift-frontend -typecheck %s -swift-version 5 %api_diff_data_dir

// Note that the diff run line indicates that there should be no change.

// The compiler should not suggest `let` instead of `var` here because
// it is a compile error to say `for let ...`.
// rdar://problem/32390726

for var i in 0..<10 {
  _ = i + 1
}
