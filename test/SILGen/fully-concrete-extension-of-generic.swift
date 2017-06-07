// RUN: %target-swift-frontend -emit-silgen -verify %s 

class C<T> {
  init() {}
}

extension C where T == Int {
  convenience init(forInt _: ()) {
    self.init()
  }
}

func exerciseInits(which: Bool) -> C<Int> {
  if which {
    return C()
  } else {
    return C(forInt: ())
  }
}
