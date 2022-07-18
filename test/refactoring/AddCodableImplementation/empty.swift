// RUN: rm -rf %t.result && mkdir -p %t.result

struct User: Codable {
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=3:8 > %t.result/codable.swift
// RUN: diff -u %S/Outputs/empty/codable.swift.expected %t.result/codable.swift
