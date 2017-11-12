private var global: String = "abc"

extension MyStruct {
  private static func method() -> String? { return nil }
}

// Note: These are deliberately 'private' here and 'internal' in the other file.
// They're testing that we don't filter out non-discriminated decls in lookup.
private func process(_ x: String) -> String { return x }
extension MyStruct {
  private static func process(_ x: String) -> String { return x }
}
