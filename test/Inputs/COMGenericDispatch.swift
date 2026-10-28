extension IItem {
  @inline(never)
  public func adjusted(_ offset: Int32) -> Int32 { value(offset) }
}

@inline(never)
public func dispatch<T: IExtended>(_ value: borrowing T) -> Int32 {
  value.value(1) + value.multiply(2)
}

@inline(never)
public func property<T: IClassItem>(_ value: borrowing T) -> Int32 {
  value.value += 1
  return value.value
}

@inline(never)
public func dispatchPack<each T: IItem>(_ values: repeat each T) -> Int32 {
  var result: Int32 = 0
  for value in repeat each values { result += value.value(0) }
  return result
}

@inline(never)
public func edit<T: IClassItem>(_ value: borrowing T,
                               _ body: (inout Int32) throws -> Void) rethrows {
  try body(&value.value)
}

public enum ValueError: Error { case negative }

extension IItem {
  @inline(never)
  public var nonnegative: Int32 {
    get throws {
      let result = value(0)
      guard result >= 0 else { throw ValueError.negative }
      return result
    }
  }
}

@inline(never)
public func readNonnegative<T: IItem>(_ value: borrowing T) throws -> Int32 {
  try value.nonnegative
}
