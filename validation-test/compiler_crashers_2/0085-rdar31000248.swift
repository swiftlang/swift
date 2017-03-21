// RUN: not --crash %target-swift-frontend -emit-silgen -primary-file %s -o /dev/null

class Base<T> {
  convenience init(count: Int) {
    self.init(count: count, designated: ())
  }

  init(count: Int, designated: ()) {
  }
}

class Derived<T> : Base<T> {
  convenience init() {
    self.init(count: 0)
  }
}
