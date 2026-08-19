// RUN: %target-swift-ide-test -print-inferred-isolation -source-filename %s | %FileCheck %s

@globalActor
actor MyActor {
  static let shared = MyActor()
}

@MainActor
final class C {
  var n = 0

  func work() async {
    // Closure that inherits @MainActor from the enclosing context.
    // CHECK:      let inheritedMain = <inferred-isolation kind="closure" isolation="@MainActor">{</inferred-isolation>
    let inheritedMain = {
      self.n += 1
    }
    inheritedMain()

    // Detached closure: nonisolated regardless of enclosing context.
    // CHECK:      Task.detached <inferred-isolation kind="closure" isolation="nonisolated">{</inferred-isolation>
    Task.detached {
      print("detached")
    }

    // Task closure inherits the actor context.
    // CHECK:      Task <inferred-isolation kind="closure" isolation="@MainActor">{</inferred-isolation>
    Task {
      self.n += 1
    }
  }
}

func freeFunc() {
  // Closure outside any isolated context -- nonisolated.
  // CHECK:      let f = <inferred-isolation kind="closure" isolation="nonisolated">{</inferred-isolation> 1 }
  let f = { 1 }
  _ = f()
}

// Closures whose isolation is *written* in the signature must not be tagged,
// since the information is already on screen.

func explicitMain() {
  // CHECK:      let g = { @MainActor in 2 }
  // CHECK-NOT:  <inferred-isolation {{[^>]*}}>{</inferred-isolation> @MainActor
  let g = { @MainActor in 2 }
  _ = g
}

func explicitCustom() {
  // CHECK:      let h = { @MyActor in 3 }
  // CHECK-NOT:  <inferred-isolation {{[^>]*}}>{</inferred-isolation> @MyActor
  let h = { @MyActor in 3 }
  _ = h
}
