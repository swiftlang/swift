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
    enumerator(at: url, includingPropertiesForKeys: nil) // expected-error {{function value passed to 'enumerator' as argument #1 could be any of '(Int) -> URL', '(String?) -> URL?' but 'URL' is expected}}
  }
}
