// RUN: not %target-swift-frontend %s -typecheck

class A : A { }

func doIt<T>(obj: AnyObject) -> T? {
  return obj as? T
}

let result: A? = doIt(A()) as A?
