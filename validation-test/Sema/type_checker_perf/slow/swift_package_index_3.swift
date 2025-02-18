// RUN: %target-typecheck-verify-swift %s -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

extension String {
  func replacingOccurrences(of: String, with: String) -> String { return "" }
  func components(separatedBy: String) -> [String] { return [] }
}

func getProperties(
    from ics: String
) -> [(name: String, value: String)] {
    return ics // expected-error {{the compiler is unable to type-check this expression in reasonable time}}
        .replacingOccurrences(of: "\r\n ", with: "")
        .components(separatedBy: "\r\n")
        .map { $0.split(separator: ":", maxSplits: 1, omittingEmptySubsequences: true) }
        .filter { $0.count > 1 }
        .map { (String($0[0]), String($0[1])) }
}
