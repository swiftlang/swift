// rdar://186306192
// RUN: %target-typecheck-verify-swift

struct FileType: Hashable {}

func small(_: [String: FileType]?) {}

func test(d: [String: String], b: Bool) {
  small(b ? nil : d)
  // expected-error@-1 {{result values in '? :' expression have mismatching types '[String : String]' and '[String : FileType]'}}
  // expected-note@-2 {{arguments to generic parameter 'Value' ('String' and 'FileType') are expected to be equal}}
}
