// RUN: %target-swift-ide-test -signature-help -code-completion-token=MEMBER_SUBSCRIPT -source-filename=%s | %FileCheck %s --check-prefix=MEMBER_SUBSCRIPT

struct Matrix {
  subscript(row: Int, column: Int) -> Int {
    return 0
  }
  
  subscript(row r: Int) -> [Int] {
    return []
  }
  
  subscript(column c: Int) -> [Int] {
    return []
  }
}

let matrix = Matrix()
matrix[#^MEMBER_SUBSCRIPT^#]
// MEMBER_SUBSCRIPT:     Begin signatures, 4 items
// MEMBER_SUBSCRIPT-DAG: Signature[Active]: subscript(<param active>keyPath: KeyPath<Matrix, Value></param>) -> Value
// MEMBER_SUBSCRIPT-DAG: Signature: subscript(<param name="row" active>row: Int</param>, <param name="column">column: Int</param>) -> Int
// MEMBER_SUBSCRIPT-DAG: Signature: subscript(<param name="r" active>row: Int</param>) -> [Int]
// MEMBER_SUBSCRIPT-DAG: Signature: subscript(<param name="c" active>column: Int</param>) -> [Int]
