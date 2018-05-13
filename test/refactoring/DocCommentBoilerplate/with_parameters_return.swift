func funcWithParamsReturnNoThrow(_ a: Int, b: Int, c: String) -> (a: Int, b: String) {
    
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -doc-comment-boilerplate -source-filename %s -pos=1:7 > %t.result/with_parameters_return.swift
// RUN: diff -u %S/Outputs/with_parameters_return/with_parameters_return.swift.expected %t.result/with_parameters_return.swift
