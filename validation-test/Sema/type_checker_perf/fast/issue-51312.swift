// RUN: %target-typecheck-verify-swift -solver-scope-threshold=300

// https://github.com/swiftlang/swift/issues/51312

extension Sequence where Element == Int {
  var stringValue: String { "" }
}

func slow() {
  let _ =
    (0x0041...0x005A).stringValue + // 'A' to 'Z'
    (0x0061...0x007A).stringValue + // 'a' to 'z'
    "_" +
    "\u{00A8}\u{00AA}\u{00AD}\u{00AF}" +
    (0x00B2...0x00B5).stringValue +
    (0x00B7...0x00BA).stringValue +
    (0x00BC...0x00BE).stringValue +
    (0x00C0...0x00D6).stringValue +
    (0x00D8...0x00F6).stringValue +
    (0x00F8...0x00FF).stringValue +
    (0x0100...0x02FF).stringValue +
    (0x0370...0x167F).stringValue +
    (0x1681...0x180D).stringValue +
    (0x180F...0x1DBF).stringValue +
    (0x1E00...0x1FFF).stringValue +
    (0x200B...0x200D).stringValue +
    (0x202A...0x202E).stringValue +
    (0x203F...0x2040).stringValue +
    "\u{2054}" +
    (0x2060...0x206F).stringValue +
    (0x2070...0x20CF).stringValue +
    (0x2100...0x218F).stringValue +
    (0x2460...0x24FF).stringValue +
    (0x2776...0x2793).stringValue +
    (0x2C00...0x2DFF).stringValue +
    (0x2E80...0x2FFF).stringValue +
    (0x3004...0x3007).stringValue +
    (0x3021...0x302F).stringValue +
    (0x3031...0x303F).stringValue +
    (0x3040...0xD7FF).stringValue +
    (0xF900...0xFD3D).stringValue +
    (0xFD40...0xFDCF).stringValue +
    (0xFDF0...0xFE1F).stringValue +
    (0xFE30...0xFE44).stringValue +
    (0xFE47...0xFFFD).stringValue +
    (0x10000...0x1FFFD).stringValue +
    (0x20000...0x2FFFD).stringValue +
    (0x30000...0x3FFFD).stringValue +
    (0x40000...0x4FFFD).stringValue +
    (0x50000...0x5FFFD).stringValue +
    (0x60000...0x6FFFD).stringValue +
    (0x70000...0x7FFFD).stringValue +
    (0x80000...0x8FFFD).stringValue +
    (0x90000...0x9FFFD).stringValue +
    (0xA0000...0xAFFFD).stringValue +
    (0xB0000...0xBFFFD).stringValue +
    (0xC0000...0xCFFFD).stringValue +
    (0xD0000...0xDFFFD).stringValue +
    (0xE0000...0xEFFFD).stringValue
}
