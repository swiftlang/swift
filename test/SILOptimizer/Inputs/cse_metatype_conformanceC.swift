import moda

// module C
extension A: P {
  public static func foo() {
    print("modC\n")
  }
} // conformance C

@inlinable
public func getPFromC() -> P.Type {
  return A.self
}
