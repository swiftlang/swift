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
extension Cat: P where Left: Copyable, Right: ~Copyable {}

struct ConditionallyCopyable<T: ~Copyable>: ~Copyable {
  var value: Int = 17
}
extension ConditionallyCopyable: Copyable where T: Copyable { }

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

  // CHECK: failed to cast (attemptCall)
  attemptCall(Dog<Noncopyable>())

  // CHECK: failed to cast (attemptCall)
  attemptCall(Dog<ConditionallyCopyable<Noncopyable>>())

  // CHECK: hello
  attemptCall(Cat<Ordinary, Noncopyable>())

  // CHECK: failed to cast (attemptCall)
  attemptCall(Cat<Noncopyable, Ordinary>())

   // CHECK: failed to cast (attemptCall)
  attemptCall(Cat<Noncopyable, ConditionallyCopyable<Ordinary>>())

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
