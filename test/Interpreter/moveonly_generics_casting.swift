// RUN: %target-run-simple-swift(-Xfrontend -sil-verify-all -enable-experimental-feature NoncopyableGenerics) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -Xfrontend -sil-verify-all -enable-experimental-feature NoncopyableGenerics) | %FileCheck %s

// REQUIRES: executable_test

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
  print("failed to cast (attemptCall)")
}

defer { main() }
func main() {
  // CHECK: hello
  attemptCall(Dog<Ordinary>())

  // FIXME: this is NOT suppose to succeed! (rdar://123466649)
  // CHECK: hello
  attemptCall(Dog<Noncopyable>())

  // CHECK: hello
  attemptCall(Cat<Ordinary, Noncopyable>())

  // FIXME: this is NOT suppose to succeed! (rdar://123466649)
  // CHECK: hello
  attemptCall(Cat<Noncopyable, Ordinary>())

  // CHECK: cast succeeded
  test_radar124171788(.nothing)
}

// coverage for rdar://124171788
enum Maybe<Wrapped: ~Copyable>: ~Copyable {
  case just(Wrapped)
  case nothing
}
extension Maybe: Copyable {}
extension Maybe: CustomDebugStringConvertible {
  var debugDescription: String {
    "cast succeeded"
  }
}
func test_radar124171788(_ v: Maybe<Int>) {
  if let foo = v as? CustomDebugStringConvertible {
    print("\(foo.debugDescription)")
    return
  }
  print("failed to cast (test_radar124171788)")
}
