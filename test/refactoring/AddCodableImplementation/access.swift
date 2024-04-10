// RUN: rm -rf %t.result && mkdir -p %t.result

private struct PrivateS: Codable {
  let value: Int
}

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=3:16 > %t.result/private.swift
// RUN: diff -u %S/Outputs/access/private.swift.expected %t.result/private.swift

public struct PublicS: Codable {
  let value: Int
}

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=10:15 > %t.result/public.swift
// RUN: diff -u %S/Outputs/access/public.swift.expected %t.result/public.swift

open class OpenC: Codable {
  let value: Int
}

// RUN: %refactor -add-explicit-codable-implementation -source-filename %s -pos=17:12 > %t.result/open.swift
// RUN: diff -u %S/Outputs/access/open.swift.expected %t.result/open.swift
