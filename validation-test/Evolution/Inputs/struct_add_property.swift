
public func getVersion() -> Int {
#if BEFORE
  return 0
#else
  return 1
#endif
}

#if BEFORE

public struct AddStoredProperty {
  public init() {
    forth = "Chuck Moore"
  }

  public var forth: String

  public var languageDesigners: [String] {
    return [forth]
  }
}

#else

public struct AddStoredProperty {
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

#if BEFORE

var global: Int = 0

public struct ChangeEmptyToNonEmpty {
  public init() {}

  public var property: Int {
    get { return global }
    set { global = newValue }
  }
}

#else

public struct ChangeEmptyToNonEmpty {
  public init() {
    property = 0 
  }

  public var property: Int
}

#endif

public func getProperty(_ c: ChangeEmptyToNonEmpty) -> Int {
  return c.property
}

