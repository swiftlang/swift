func funcWithNoParamsNoReturnNoThrow() {
    
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=1:7 > %t.result/basic_1.swift
// RUN: diff -u %S/Outputs/basic/basic_1.swift.expected %t.result/basic_1.swift
