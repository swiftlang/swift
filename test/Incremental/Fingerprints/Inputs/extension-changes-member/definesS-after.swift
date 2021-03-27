public struct S {
  private
  static func foo(_ i: Int) {print("1: other:2 commented out")}
}
extension S {
  // private // commented out to ensure we see a change to the fingerprint
  static func foo2(_ i: Int) {print("2: other:6 commented out")}
}
