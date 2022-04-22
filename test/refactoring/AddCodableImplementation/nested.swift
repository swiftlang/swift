// RUN: rm -rf %t.result && mkdir -p %t.result
struct Response: Codable {
  let pages: [Page]

  struct Page: Codable {
    let results: [Result]

    struct Result: Codable {
      let title: String
    }
  }
}

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=8:12 > %t.result/codable.swift
// RUN: diff -u %S/Outputs/nested/codable.swift.expected %t.result/codable.swift
