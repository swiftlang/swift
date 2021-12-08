import ExperimentalRegexBridging

public func experimental_regex_strawperson(
  _ ptr: UnsafePointer<CChar>?
) -> UnsafePointer<CChar>? {
  guard let s = ptr else { return nil }

  func error(_ str: String) -> UnsafePointer<CChar>? {
    let count = str.utf8.count + 1
    return str.withCString {
      assert($0[count-1] == 0)
      let ptr = UnsafeMutablePointer<CChar>.allocate(capacity: count)
      ptr.initialize(from: $0, count: count)
      return UnsafePointer(ptr)
    }
  }

  let str = String(cString: s)
  if str.starts(with: "*") || str.starts(with: "+") || str.starts(with: "?") {
    return error("cannot start regex with quantifier")
  }
  return nil
}

public func registerParser() {
  Parser_registerParseRegexStrawperson({ experimental_regex_strawperson($0) })
}
