// rdar://123947239
// RUN: %target-typecheck-verify-swift

struct DispatchTime {
  static func now() -> DispatchTime { DispatchTime() }
}
enum DispatchTimeInterval {
  case seconds(Int)
  case milliseconds(Int)
}
func +(time: DispatchTime, interval: DispatchTimeInterval) -> DispatchTime { time }
func +(time: DispatchTime, seconds: Double) -> DispatchTime { time }

class DispatchQueue {
  static let main = DispatchQueue()
  func asyncAfter(deadline: DispatchTime, execute work: @escaping () -> Void) {}
}

func test() {
  DispatchQueue.main.asyncAfter(deadline: .now + .seconds(0.25)) {}
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
