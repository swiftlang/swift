import Foundation

class B {
  func f() -> Bool {
    let now = NSDate()
    let later = NSDate.distantFuture as NSDate
    return now.compare(later as Date) != .orderedDescending
  }
}

print(B().f())
