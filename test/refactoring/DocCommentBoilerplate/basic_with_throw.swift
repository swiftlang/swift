func funcWithNoReturnNoParamsThrow() throws{
    
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=1:7 > %t.result/basic_with_throw.swift
// RUN: diff -u %S/Outputs/basic_with_throw/basic_with_throw.swift.expected %t.result/basic_with_throw.swift
