public enum TrafficLight {
  case Red
  case Yellow
  case Green
}

public enum Term<T, U> {
  case Value(U)
  case Function(T, U)
}

public enum Phantom<T> : Int {
  case Up
  case Down
}
