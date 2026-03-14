// RUN: %empty-directory(%t) 
// RUN: %target-build-swift %s -emit-executable -enable-experimental-feature BorrowAndMutateAccessors -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: executable_test

public struct Container<Element: ~Copyable >: ~Copyable {
  var _storage: UnsafeMutableBufferPointer<Element>
  var _count: Int

  var first: Element {
    @_unsafeSelfDependentResult
    borrow {
      return _storage.baseAddress.unsafelyUnwrapped.pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      return &_storage.baseAddress.unsafelyUnwrapped.pointee
    }
  }

  public subscript(index: Int) -> Element {
    @_unsafeSelfDependentResult
    borrow {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return _storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
    @_unsafeSelfDependentResult
    mutate {
      precondition(index >= 0 && index < _count, "Index out of bounds")
      return &_storage.baseAddress.unsafelyUnwrapped.advanced(by: index).pointee
    }
  }
}

extension Container: Copyable where Element: Copyable {}

func test() {
  let n = 10_000
  var arr = Array(0...n)
  let count = arr.count
  let sum = arr.withUnsafeMutableBufferPointer { ubpointer in
    let container = Container(_storage: ubpointer, _count: count)
    var sum = 0
    for i in 0..<container._count {
      sum &+= container[i]
    }
    return sum
  }
  let expectedSum = n * (n + 1) / 2
  assert(sum == expectedSum)
  let mutated_sum = arr.withUnsafeMutableBufferPointer { ubpointer in
    var container = Container(_storage: ubpointer, _count: count)
    var sum = 0
    for i in 0..<container._count {
      container[i] &+= 1
      sum += container[i]
    }
    return sum
  }
  let mutatedExpectedSum = (n + 1) * (n + 2) / 2
  assert(mutated_sum == mutatedExpectedSum)
}

test()

