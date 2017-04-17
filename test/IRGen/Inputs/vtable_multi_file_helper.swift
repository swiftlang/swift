class EmptyBase {}
class Subclass : EmptyBase {
  struct String { 
    var owner: AnyObject? = nil
    var start = 0, end = 0
  }
  class var classProp: String { return String() }
}
