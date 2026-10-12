@com(interface: "10000000-0000-0000-0000-000000000001")
public protocol IItem {
  func value(_ offset: Int32) -> Int32
}

@com(interface: "10000000-0000-0000-0000-000000000002")
public protocol IExtended: IItem {
  func multiply(_ factor: Int32) -> Int32
}

@inline(never)
public func size<T: IItem>(_ value: borrowing T) -> Int {
  MemoryLayout<T>.size
}

@inline(never)
public func forward<T: IExtended>(_ value: borrowing T) -> Int {
  size(value)
}

public struct Holder<T: IItem> {
  public var value: T
  public init(_ value: T) { self.value = value }
  @inline(never) public func size() -> Int { Library.size(value) }
}

public class Owner<T: IItem> {
  public var value: T
  public init(_ value: T) { self.value = value }
  @inline(never) public func size() -> Int { Library.size(value) }
}

@inline(never)
public func capture<T: IItem>(_ value: T) -> () -> Int {
  { size(value) }
}

@inline(never)
public func pack<each T: IItem>(_ values: repeat each T) -> Int {
  var result = 0
  for value in repeat each values { result += size(value) }
  return result
}

@inline(never)
public func forwardPack<each T: IItem>(_ values: repeat each T) -> Int {
  pack(repeat each values)
}

@com(interface: "10000000-0000-0000-0000-000000000003")
public protocol IClassItem: AnyObject {
  var value: Int32 { get set }
}

@inline(never)
public func captureClass<T: IClassItem>(_ value: T) -> () -> Int {
  { withExtendedLifetime(value) { MemoryLayout<T>.size } }
}
