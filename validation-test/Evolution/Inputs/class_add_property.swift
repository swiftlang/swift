
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if BEFORE

public final class AddStoredProperty {
  public init() {
    forth = "Chuck Moore"
  }

  public var forth: String

  public var languageDesigners: [String] {
    return [forth]
  }
}

#else

public final class AddStoredProperty {
  public init() {
    forth = "Chuck Moore"
    lisp = "John McCarthy"
    c = "Dennis Ritchie"
  }

  public var forth: String
  public var lisp: String
  public var c: String

  public var languageDesigners: [String] {
    return [forth, lisp, c]
  }
}

#endif
