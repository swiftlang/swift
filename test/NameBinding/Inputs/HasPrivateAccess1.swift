private var global: Int = 1

public struct MyStruct {
  private static func method() -> Int? { return nil }
}

// Note: These are deliberately 'internal' here and 'private' in the other file.
// They're testing that we don't filter out non-discriminated decls in lookup.
internal func process(_ x: Int) -> Int { return x }
extension MyStruct {
  internal static func process(_ x: Int) -> Int { return x }
}
