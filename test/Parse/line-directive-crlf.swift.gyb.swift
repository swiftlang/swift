// RUN: %empty-directory(%t)
// RUN: %gyb %t/line-directive-crlf.swift
// RUN: %target-typecheck-verify-swift
// RUN: not %target-swift-frontend -c %t/line-directive-crlf.swift 2>&1 | %FileCheck %t/line-directive-crlf.swift

// https://github.com/apple/swift/issues/57553
func I57553() {
% print("#sourceLocation(file: \"issue-57553.swift\", line: 12345)\r\n")
% print("#sourceLocation()\r\n")
}
