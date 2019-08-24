// RUN: not %target-swift-frontend %s -typecheck

enum State<StateType> {
  func put<StateType>() -> StateType {}
  func put<T>(keyPath: WritableKeyPath<StateType, T>, projection: T) {
    put(keyPath: \.age, projection: {})
  }
}
