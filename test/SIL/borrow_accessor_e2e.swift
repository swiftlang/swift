// RUN: %empty-directory(%t) 
// RUN: %target-build-swift %s -emit-executable -enable-experimental-feature BorrowAndMutateAccessors -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: executable_test

public struct Container<Element: ~Copyable >: ~Copyable {
  var _storage: UnsafeBufferPointer<Element>
  var _count: Int

  var first: Element {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.baseAddress.unsafelyUnwrapped.pointee
    }
  }

  public subscript(index: Int) -> Element {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

extension Container: Copyable where Element: Copyable {}

func test() {
  let n = 10_000
  let arr = Array(0...n)
  let sum = arr.withUnsafeBufferPointer { ubpointer in
    let container = Container(_storage: ubpointer, _count: arr.count)
    var sum = 0
    for i in 0..<container._count {
      sum &+= container[i]
    }
    return sum
  }
  let expectedSum = n * (n + 1) / 2
  assert(sum == expectedSum)  
}

test()

