// RUN: %target-typecheck-verify-swift -dump-parse

// https://github.com/apple/swift/issues/61806

struct I61806 {}
extension I61806 {
  init() { }
}

class I61806_C { 
  init() {}
}
extension I61806_C {
  convenience init(a: Int) {}
}

protocol I61806_P {}
extension I61806_P {
 init() {}
}
