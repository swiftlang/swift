// RUN: not %target-swift-frontend -typecheck %s

typealias IndexResult = Result<Bol, Error>
extension IndexResult {
  func perfect() -> Self {
    Success(true)
  }
}
