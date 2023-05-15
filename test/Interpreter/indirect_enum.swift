// RUN: %target-swiftc_driver %s -g -sanitize=address -o %t_asan-binary
// RUN: %target-codesign %t_asan-binary
// RUN: env ASAN_OPTIONS=detect_leaks=0 %target-run %t_asan-binary

// REQUIRES: executable_test
// REQUIRES: asan_runtime

// Make sure that we do not use-after-free here.

class Box<T> {
  var value: T
  init(_ inputValue: T) { value = inputValue }
}

enum Value<U> {
  case inline(U)
  indirect case box(Box<U>)
}

func evaluate<U>(_ v: Value<U>) {
  switch v {
  case .inline:
    return print("foo")
  case .box(let box):
    return print("bar \(box)")
  }
}

func main() {
  let v = Value.box(Box<[Int]>([1, 2, 3]))
  evaluate(v)
  print(v)
}

main()

