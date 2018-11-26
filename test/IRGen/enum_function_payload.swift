// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir %s

enum Singleton {
  case F((Singleton) -> ())
}

enum Single {
  case F((Single) -> ())
  case X
  case Y
}

enum Multi {
  case F((Multi) -> ())
  case G((Multi) -> ())
}

enum Autoclosure<T> {
  case first(@autoclosure () -> Bool, T)
  case second(Int, @autoclosure () -> T)
}

_ = Autoclosure.first(false, 3)
_ = Autoclosure.second(3, "hi")
