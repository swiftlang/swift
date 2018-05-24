class Person {
    init(firstName: String, lastName: String, age: Int) {
        
    }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=2:7 > %t.result/constructor_with_params.swift
// RUN: diff -u %S/Outputs/constructor_with_params/constructor_with_params.swift.expected %t.result/constructor_with_params.swift
