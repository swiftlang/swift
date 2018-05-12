func funcWithParamNoReturnNoThrow(_ a: Int) {
    
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=1:7 > %t.result/with_parameter.swift
// RUN: diff -u %S/Outputs/basic/with_parameter.swift.expected %t.result/with_parameter.swift
