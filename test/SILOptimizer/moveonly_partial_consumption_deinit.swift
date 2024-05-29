// RUN: %target-swift-emit-sil \
// RUN:     %s \
// RUN:     -sil-verify-all \
// RUN:     -verify

struct Ur : ~Copyable {
  deinit {}
}

struct Regular : ~Copyable {
  var u0: Ur
  var u1: Ur

  deinit {
    borrowNoncopyable(self)
    takeUr(u0)
    takeUr(u1)
  }
}

struct RegularGeneric<T> : ~Copyable {
  deinit{
    bye()
    borrowNoncopyable(self)
    take(t)
    borrowUr(ur)
    takeUr(ur)
  }
  var ur: Ur
  var t: T
}

struct NoncopyableGeneric<T : ~Copyable> : ~Copyable {
  deinit{
    bye()
    borrowNoncopyable(self)
    borrowNoncopyable(t)
    takeNoncopyable(t)
    borrowUr(ur)
    takeUr(ur)
  }
  var ur: Ur
  var t: T
}

func bye() {}

func borrowNoncopyable<T : ~Copyable>(_ t: borrowing T) {}
func takeNoncopyable<T : ~Copyable>(_ t: consuming T) {}

func borrow<T>(_ t: borrowing T) {}
func take<T>(_ t: consuming T) {}

func borrowUr(_ s: borrowing Ur) {}
func takeUr(_ t: consuming Ur) {}

func swapUr(_ u0: inout Ur, _ u1: inout Ur) {
  let tmp = u0
  u0 = u1
  u1 = tmp
}
