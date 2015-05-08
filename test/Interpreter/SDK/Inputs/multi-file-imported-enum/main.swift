import Foundation

class B {
  func f() -> Bool {
    let now = NSDate()
    let later = NSDate.distantFuture() as! NSDate
    return now.compare(later) != .OrderedDescending
  }
}

print(B().f())
