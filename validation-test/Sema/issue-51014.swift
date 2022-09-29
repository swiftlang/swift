// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-swift-frontend -primary-file %t/main.swift %S/Inputs/issue-51014-helper.swift -emit-ir -o /dev/null

// https://github.com/apple/swift/issues/51014

StateAnimatorGroup<String>().set("")
