func funcWithReturnNoParamsNoThrow() -> [Int]{
    
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=1:7 > %t.result/basic_with_return.swift
// RUN: diff -u %S/Outputs/basic/basic_with_return.swift.expected %t.result/basic_with_return.swift
