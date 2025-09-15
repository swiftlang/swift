// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

struct Time {
  static func +(_: Time, _: Double) -> Time {
    Time()
  }

  static func now() -> Time { Time() }
}

struct Queue {
  static let current = Queue()

  func after(deadline: Time, execute: () -> Void) {}
  func after(deadline: Time, execute: (Int) -> Void) {}
}

func compute(_: () -> Void) {}

protocol P {
}

@resultBuilder
struct Builder {
  static func buildExpression<T: P>(_ v: T) -> T {
    v
  }

  static func buildBlock<T: P>(_ v: T) -> T {
    v
  }

  static func buildBlokc<T0: P, T1: P>(_ v0: T0, _ v1: T1) -> (T0, T1) {
    (v0, v1)
  }
}

struct MyP: P {
  func onAction(_: () -> Void) -> some P { self }
}

class Test {
  var value = 0.0

  @Builder func test() -> some P {
    MyP().onAction {
      Queue.current.after(deadline: .now() + 0.1) {
        compute {
          value = 0.3
          Queue.current.after(deadline: .now() + 0.2) {
            compute {
              value = 1.0
              Queue.current.after(deadline: .now() + 0.2) {
                compute {
                  value = 0.3
                  Queue.current.after(deadline: .now() + 0.2) {
                    compute {
                      value = 1.0
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
