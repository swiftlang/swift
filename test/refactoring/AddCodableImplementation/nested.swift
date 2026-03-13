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

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=8:12 > %t.result/inner.swift
// RUN: diff -u %S/Outputs/nested/inner.swift.expected %t.result/inner.swift

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=2:8 > %t.result/outer.swift
// RUN: diff -u %S/Outputs/nested/outer.swift.expected %t.result/outer.swift
