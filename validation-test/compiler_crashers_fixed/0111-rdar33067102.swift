// RUN: %target-swift-frontend -swift-version 4 %s -typecheck

func flatterMap(_ records: [(Int)]) -> [Int] {
  records.flatMap { _ in return 1 } // expected-note {{}}
}
