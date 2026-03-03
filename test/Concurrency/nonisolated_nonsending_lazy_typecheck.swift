// %empty-directory(%t)
// RUN: split-file %s %t

// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// Test both with and without '-experimental-lazy-typecheck'
// RUN: %swift-frontend -emit-module %t/Lib.swift -swift-version 6 -experimental-lazy-typecheck -experimental-skip-all-function-bodies -enable-upcoming-feature NonisolatedNonsendingByDefault -module-name Lib -o %t/Modules/Lib.swiftmodule
// RUN: %swift-frontend -emit-sil %t/Test.swift -I %t/Modules -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault

// RUN: %swift-frontend -emit-module %t/Lib.swift -swift-version 6 -experimental-skip-all-function-bodies -enable-upcoming-feature NonisolatedNonsendingByDefault -module-name Lib -o %t/Modules/Lib.swiftmodule
// RUN: %swift-frontend -emit-sil %t/Test.swift -I %t/Modules -swift-version 6 -enable-upcoming-feature NonisolatedNonsendingByDefault

//--- Lib.swift

public final class IndexStoreDownloadManager2: Sendable {
  public func withIndexStore(
    body: (String) async throws -> Void
  ) async throws {
  }
}

//--- Test.swift

import Lib

private func `import`(
  indexStoreDownloadManager: IndexStoreDownloadManager2,
) async throws {
  import2(
    indexStoreProvider: { runWithIndexStore in
      // Make sure we don't error here since `withIndexStore` is nonisolated(nonsending)
      try await indexStoreDownloadManager.withIndexStore(body: runWithIndexStore)
    }
  )
}

func import2(
  indexStoreProvider: (_ runWithIndexStore: (_ indexStore: String) async throws -> Void) async throws -> Void,
) {
}
