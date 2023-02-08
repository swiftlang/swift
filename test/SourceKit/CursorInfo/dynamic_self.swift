class UserCollection {
  static let staticMember = "ABC"
  func test() {
    // RUN: %sourcekitd-test -req=cursor -pos=%(line + 1):10 %s -- %s
    Self.staticMember
  }
}
