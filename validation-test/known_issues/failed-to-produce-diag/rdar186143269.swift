// rdar://186143269
// RUN: %target-typecheck-verify-swift

struct QFile: TextOutputStream {
  func write(_ string: String) {}
}

extension TextOutputStream where Self == QFile {
  static var stderr: QFile { get { QFile() } set {} }
}

func test() {
  print("OK", to: &.stderr)
  print("NG", to: .stderr) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
}
