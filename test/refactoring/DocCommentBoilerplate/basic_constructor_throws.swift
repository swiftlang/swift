class Person {
    init() throws{
        
    }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=2:7 > %t.result/basic_constructor_throws.swift
// RUN: diff -u %S/Outputs/basic_constructor_throws/basic_constructor_throws.swift.expected %t.result/basic_constructor_throws.swift
