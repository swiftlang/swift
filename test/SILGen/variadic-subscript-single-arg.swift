// RUN: %target-swift-frontend -emit-silgen -verify %s

struct Butt {
  subscript(butts: Int...) -> Int {
    return 0
  }
}

_ = Butt()[1]
