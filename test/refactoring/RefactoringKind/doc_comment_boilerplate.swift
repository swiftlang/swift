func addNumber(first: Int, second: Int) throws -> Int {
    return first + second
}

// RUN: %refactor -source-filename %s -pos=1:6 | %FileCheck %s -check-prefix=CHECK-DOC-COMMENT-BP
// CHECK-DOC-COMMENT-BP: Generate Documentation Comment
