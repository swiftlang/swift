public var x: Int = 0

public enum Choice {
  case yes, no, maybeSo
}

public typealias Callback = () -> Void

public typealias Pair<T> = (T, T)
