// RUN: %target-typecheck-verify-swift -disable-availability-checking -parse-as-library
// This should pass without any warnings or errors

@MainActor
var floofer: Int = 42

@main struct Doggo { }

extension Doggo {
  static func main() {
    print("Doggo value: \(floofer)")
  }
}
