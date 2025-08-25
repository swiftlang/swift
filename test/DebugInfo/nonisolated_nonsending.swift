// RUN: %target-swift-frontend -emit-ir -g %s -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

public protocol P: Sendable {
  func f(_: Int) async
}

public struct S: Sendable {
  public func f(p: [any P]) async {
    let x: [@Sendable (Int) async -> ()] = p.map { $0.f }

    // FIXME: crashes
    //let y = p.map { $0.f }
  }
}
