// RUN: %target-parse-verify-swift

// ---------------------------------------------------------------------------
// Mark function's return value as discardable and silence warning
// ---------------------------------------------------------------------------
@discardableResult func f1() -> [Int] { }

class C1 {
  @discardableResult init() { }

  @discardableResult
  func f1() -> Int { }
}

struct Inits1 {
  @discardableResult init() { }
}
