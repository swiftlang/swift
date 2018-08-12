protocol Proto {
  var prop: Int { get set }
}
extension Proto {
  func method() {}
}

var globalExistential: Proto {
  fatalError()
}
