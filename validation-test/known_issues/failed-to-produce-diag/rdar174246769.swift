// rdar://174246769
// RUN: %target-typecheck-verify-swift

struct URL {}

class FileManager {
  func enumerator(at url: URL, includingPropertiesForKeys keys: [String]?) {}

  func url(for directory: Int) -> URL { URL() }
  func url(forUbiquityContainerIdentifier: String?) -> URL? { nil }
}

extension FileManager {
  func foo() {
    enumerator(at: url, includingPropertiesForKeys: nil) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
