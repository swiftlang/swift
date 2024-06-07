// RUN: %empty-directory(%t.result)

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=%(line + 2):8 > %t.result/invalid_member.swift
// RUN: diff -u %S/Outputs/has_error/invalid_member.swift.expected %t.result/invalid_member.swift
struct Foo: Codable {
    var other: Unknown
}

