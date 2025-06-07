import Builtin

extension Foo where T == Int32 {

}

public func foo(_: borrowing Foo<Int32>) {}

@_rawLayout(size: 4, alignment: 4)
public struct Int32Fake: ~Copyable {
  var address: UnsafeMutablePointer<Int32> {
    .init(Builtin.unprotectedAddressOfBorrow(self))
  }
}

@_rawLayout(like: T)
public struct UnsafeCell<T>: ~Copyable {
  var address: UnsafeMutablePointer<T> {
    .init(Builtin.unprotectedAddressOfBorrow(self))
  }
}

@_rawLayout(likeArrayOf: T, count: 3)
public struct SmallVectorOf3<T>: ~Copyable {
  var address: UnsafeMutablePointer<T> {
    .init(Builtin.unprotectedAddressOfBorrow(self))
  }
}
