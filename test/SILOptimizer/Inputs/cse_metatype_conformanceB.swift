import moda

// module B
extension A: P {
  public static func foo() {
    print("modA\n")
  }
} // conformance B

@inlinable
public func getPFromB() -> P.Type {
  return A.self
}
