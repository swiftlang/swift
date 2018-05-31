class Person {
    init() {
        
    }
    
    subscript(int: Int) -> Double {
        return 0.0
    }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=6:7 > %t.result/basic_subscript.swift
// RUN: diff -u %S/Outputs/basic_subscript/basic_subscript.swift.expected %t.result/basic_subscript.swift
