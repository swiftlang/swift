class Person {
    init?(firstName: String?) {
        
    }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=2:7 > %t.result/optional_constructor_with_param.swift
// RUN: diff -u %S/Outputs/optional_constructor_with_param/optional_constructor_with_param.swift.expected %t.result/optional_constructor_with_param.swift
