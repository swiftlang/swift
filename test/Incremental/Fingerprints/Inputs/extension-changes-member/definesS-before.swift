public struct S {
  private // commenting out this line works
  static func foo(_ i: Int) {print("1: other:2 commented out")}
}
extension S {
  private // commenting out this line fails
  static func foo2(_ i: Int) {print("2: other:6 commented out")}
}
