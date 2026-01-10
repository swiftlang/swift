import Foundation

extension EmitTest {
  @objc static func emit(_ number : Int) -> Int {
  // expected-note@-1 {{'emit' previously declared here}}
    return 0
  }
}
