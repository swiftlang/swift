// Used from test/IRGen/objc_enum_multi_file.swift

@objc public enum Foo: Int32 {
  case A, B, C
}

@objc public enum Bar: Int32 {
  case A = 5, B, C
}
