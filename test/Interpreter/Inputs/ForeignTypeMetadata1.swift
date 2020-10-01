import Foundation

func use(_ closure: @escaping (Int) -> ()) {}

public func captureRange(_ r: NSRange?) {
  var l = r
  use {
    if $0 == 0 {
      l = NSRange()
    }
  }
}
