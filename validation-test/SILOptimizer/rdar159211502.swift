// RUN: %target-build-swift %s -O -Xfrontend -sil-verify-all

protocol PA<I> {
  associatedtype I: P
  func get() -> I
}

protocol P {
  var pa: any PA<Self> { get }
}

func f(_ p: any P) {
  p.pa.get()
}
