// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all -enable-experimental-feature NoncopyableGenerics) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all -enable-experimental-feature NoncopyableGenerics) | %FileCheck %s

protocol P {
  func speak()
}

extension P {
  func speak() { print("hello") }
}

struct Noncopyable: ~Copyable {}
struct Ordinary {}

struct Dog<T: ~Copyable>: Copyable {}
extension Dog: P where T: Copyable {}

enum Cat<Left: ~Copyable, Right: ~Copyable>: Copyable {
    case meows
    init() { self = .meows }
}
extension Cat: P where Left: Copyable {}

func attemptCall(_ a: Any) {
  if let value = a as? P {
    value.speak()
    return
  }
  print("failed to cast")
}

defer { main() }
func main() {
  // CHECK: hello
  attemptCall(Dog<Ordinary>())

  // CHECK: failed to cast
  attemptCall(Dog<Noncopyable>())

  // FIXME: this is suppose to succeed! (rdar://123466649)
  // CHECK: failed to cast
  attemptCall(Cat<Ordinary, Noncopyable>())

  // CHECK: failed to cast
  attemptCall(Cat<Noncopyable, Ordinary>())
}
