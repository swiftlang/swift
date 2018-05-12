func funcWithParamsNoReturnNoThrow(_ a: Int, b: Int, c: String) {
    
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=1:7 > %t.result/with_parameters.swift
// RUN: diff -u %S/Outputs/basic/with_parameters.swift.expected %t.result/with_parameters.swift
