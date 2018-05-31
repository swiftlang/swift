class Person {
    init() {
        
    }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=2:7 > %t.result/basic_constructor.swift
// RUN: diff -u %S/Outputs/basic_constructor/basic_constructor.swift.expected %t.result/basic_constructor.swift
