// RUN: not %target-swift-frontend -typecheck %s

public func trichotomy<T : Comparable>(x: T, y: T) -> some Comparable {
  if x < y { return -1 } 
  else if x == y { return 0 }
  return 1
}

public func myTri<T: Comparable, U: Comparable> (retval: UnsafeMutablePointer<U>, x: UnsafeMutablePointer<T>, y: UnsafeMutablePointer<T>) {
  retval.initialize(to: trichotomy(x: x, y: y))
}

