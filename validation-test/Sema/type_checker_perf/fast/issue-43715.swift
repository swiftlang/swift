// RUN: %target-typecheck-verify-swift -solver-scope-threshold=300

func slow(someOptionalString: String?) {
  print("\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")\(someOptionalString ?? "")")
}
