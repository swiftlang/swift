// RUN: %target-swift-emit-silgen -Xllvm -sil-full-demangle %s

class GenericParent<T> {
  func doesThing(_ x: T) { print(x) }
}

class SpecializedChild : GenericParent<Int> {
  override func doesThing(_ x: Int) { print(x) }
}

let a = SpecializedChild().doesThing

a(4)
