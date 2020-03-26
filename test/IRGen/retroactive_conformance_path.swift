// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: CPU=arm64 || CPU=x86_64

// Check that the ASTMangler does not crash when mangling a retroactive conformance
// and also do a executable test to make sure the code works as expected.

extension Result: RandomAccessCollection, BidirectionalCollection, Collection, Sequence where Success: RandomAccessCollection, Success.Index == Int {
  public typealias Element = Result<Success.Element, Failure>
  public typealias Index = Int
  public var startIndex: Int {
    switch self {
      case .success(let array):
        return array.startIndex
      case .failure:
        return 0
    }
  }
   public var endIndex: Int {
    switch self {
      case .success(let array):
        return array.endIndex
      case .failure:
        return 1
    }
   }
   public subscript(position: Int) -> Result<Success.Element, Failure> {
    switch self {
    case .success(let array):
      return .success(array[position])
    case .failure(let error):
      return .failure(error)
    }
   }
}

let coll: [Int] = [4, 8, 15, 16, 23, 42]
let result: Result<[Int], Error> = .success(coll)
// CHECK: success(15)
print(result[2])
