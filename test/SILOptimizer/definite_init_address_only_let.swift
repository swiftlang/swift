// RUN: %target-swift-frontend -emit-sil -verify %s

func foo<T>(a: Bool, t: T) {
  let x: T
  defer { print(x) }

  x = t
  return
}
