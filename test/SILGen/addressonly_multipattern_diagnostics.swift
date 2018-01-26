// RUN: %target-swift-frontend %s -o /dev/null -emit-silgen -verify

protocol GestureData {}
struct PanData : GestureData {}
struct PinchData : GestureData {}

enum Gesture {
case pan(PanData)
case pinch(PinchData)
}

func testProtocolType(_ a : Gesture) {
  switch a {
  case .pan(let data as GestureData), // expected-error {{matching a protocol value in multiple patterns is not yet supported; use separate cases instead}}
       .pinch(let data as GestureData):
    print(data)
  }

  // This switch makes sure that we preserve the CFG so that dead code warnings do not show up. It also ensures that in at least two cases, we get one error per switch.
  switch a {
  case .pan(let data as GestureData), // expected-error {{matching a protocol value in multiple patterns is not yet supported; use separate cases instead}}
       .pinch(let data as GestureData):
    print(data)
  }
}

func testGenericType<T, T2>(_ t : T, _ t2 : T2, _ a : Any, _ b : Any) -> T? {
  switch (a, b) {
  case (let x as T, _), // expected-error {{matching a generic value in multiple patterns is not yet supported; use separate cases instead}}
       (_, let x as T):
    return x
    // This warning check is to ensure that we allow for warnings to be emitting in case blocks.
    print("found it!") // expected-warning {{code after 'return' will never be executed}}
  case (let x as T, let y as T2):
    print(x)
    print(y)
    break
  default:
    return nil
    // This warning check is to ensure that we allow for warnings to be emitting in case blocks.
    print("we failed = (")  // expected-warning {{code after 'return' will never be executed}}
  }

  return nil
  print("we failed = (")  // expected-warning {{code after 'return' will never be executed}}
}
