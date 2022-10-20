// RUN: %target-typecheck-verify-swift -enable-experimental-variadic-generics

func tuplify<T...>(_ t: T...) -> (T...) {
  return (t...)
}

func prepend<First, Rest...>(value: First, to rest: Rest...) -> (First, Rest...) {
  return (value, rest...)
}

func concatenate<T..., U...>(_ first: T..., with second: U...) -> ((T, U)...) {
  return ((first, second)...)
}

func zip<T..., U...>(_ first: T..., with second: U...) -> ((T, U)...) {
  return ((first, second)...)
}
