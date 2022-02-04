// RUN: rm -rf %t.result && mkdir -p %t.result

enum Payload: Codable {
  case plain(String)
  case pair(key: String, value: String)
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=3:8 > %t.result/codable.swift
// RUN: diff -u %S/Outputs/enum/codable.swift.expected %t.result/codable.swift

enum Payload_D: Decodable {
  case plain(String)
  case pair(key: String, value: String)
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=10:8 > %t.result/decodable.swift
// RUN: diff -u %S/Outputs/enum/decodable.swift.expected %t.result/decodable.swift

enum Payload_E: Encodable {
  case plain(String)
  case pair(key: String, value: String)
}
// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=17:8 > %t.result/encodable.swift
// RUN: diff -u %S/Outputs/enum/encodable.swift.expected %t.result/encodable.swift
