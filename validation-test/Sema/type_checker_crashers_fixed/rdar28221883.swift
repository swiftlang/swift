// RUN: not %target-swift-frontend %s -typecheck

typealias F = (inout Int?) -> Void

class C {
  var s: [String : Any?] = [:]
}

class K<T> {
  init(with: @escaping (T, F) -> Void) {}
}

// Related: SR-2994
_ = K{ (c: C?, fn: F) in fn(&(c.s["hi"])) }
