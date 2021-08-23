public protocol P {}

public enum E<First, Second> {
  case left(First)
  case right(Second)
}
