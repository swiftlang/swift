protocol ObservableType {
  func subscribe<O>(_: O)
}

protocol ConnectableObservableType : ObservableType {
  func connect()
}
