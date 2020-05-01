class TestAddEquatable {
    var property = "test"
    private var prop = "test2"
    let pr = "test3"
}

extension TestAddEquatable {
    func test() -> Bool {
        return true
    }
}

extension TestAddEquatable {
}

// RUN: rm -rf %t.result && mkdir -p %t.result

// RUN: %refactor -add-equatable-conformance -source-filename %s -pos=1:16 > %t.result/first.swift
// RUN: diff -u %S/Outputs/basic/first.swift.expected %t.result/first.swift

// RUN: %refactor -add-equatable-conformance -source-filename %s -pos=7:13 > %t.result/second.swift
// RUN: diff -u %S/Outputs/basic/second.swift.expected %t.result/second.swift

// RUN: %refactor -add-equatable-conformance -source-filename %s -pos=13:13 > %t.result/third.swift
// RUN: diff -u %S/Outputs/basic/third.swift.expected %t.result/third.swift
