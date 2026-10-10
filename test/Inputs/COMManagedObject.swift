private import Builtin

@unsafe
public enum ManagedObject<Interface> where Interface.Type: COMInterface {
  @_transparent
  public static func takeRetainedValue(_ pointer: UnsafeMutableRawPointer)
      -> Interface {
    return Builtin.takeFromRawPointer(pointer._rawValue)
  }

  @_transparent
  public static func takeRetainedValue(_ pointer: UnsafeMutableRawPointer?)
      -> Interface? {
    guard let pointer else { return nil }
    return takeRetainedValue(pointer)
  }

  @_transparent
  public static func takeRetainedValue<Pointee>(_ pointer: UnsafeMutablePointer<Pointee>?)
      -> Interface? {
    guard let pointer else { return nil }
    return takeRetainedValue(UnsafeMutableRawPointer(pointer))
  }

  @_transparent
  public static func passUnretained(_ interface: borrowing Interface)
      -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(Builtin.bridgeToRawPointer(interface))
  }
}
