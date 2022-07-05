// RUN: rm -rf %t.result && mkdir -p %t.result

struct User: Codable {
  let firstName: String
  let lastName: String?
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=3:8 > %t.result/codable.swift
// RUN: diff -u %S/Outputs/basic/codable.swift.expected %t.result/codable.swift

struct User_D: Decodable {
  let firstName: String
  let lastName: String?
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=10:8 > %t.result/decodable.swift
// RUN: diff -u %S/Outputs/basic/decodable.swift.expected %t.result/decodable.swift

struct User_E: Encodable {
  let firstName: String
  let lastName: String?
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=17:8 > %t.result/encodable.swift
// RUN: diff -u %S/Outputs/basic/encodable.swift.expected %t.result/encodable.swift
