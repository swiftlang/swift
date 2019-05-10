extension Thing : Hashable {
  func hash(into hasher: inout Hasher) {
    hasher.combine(value)
  }
}
