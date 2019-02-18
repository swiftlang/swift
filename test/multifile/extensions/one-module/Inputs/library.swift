extension Collection where Iterator.Element : Equatable {
  func split(around delimiter: [Iterator.Element]) -> ([Iterator.Element], [Iterator.Element]?) { }
}
