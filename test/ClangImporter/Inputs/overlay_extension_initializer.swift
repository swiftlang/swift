@_exported import ImageInitializers

extension Image : _ExpressibleByImageLiteral {
  private convenience init!(failableImageLiteral name: String) {
    self.init(named: .init(name))
  }

  @nonobjc
  public required convenience init(imageLiteralResourceName name: String) {
    self.init(failableImageLiteral: name)
  }
}
