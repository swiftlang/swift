// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple %s -o /dev/null -import-objc-header %S/Inputs/transfernonsendable_objc.h -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -target %target-swift-5.1-abi-triple %s -o /dev/null -import-objc-header %S/Inputs/transfernonsendable_objc.h -enable-upcoming-feature GlobalActorIsolatedTypesUsability -enable-upcoming-feature NonisolatedNonsendingByDefault

// REQUIRES: objc_interop
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

import Foundation

extension MyNotificationCenter {
  static var value = MyNotificationCenter()
}

public func handleFile(at location: URL) throws {
//  createDownloadDirectoryIfRequired()
  let movedFileLocation = try moveFile(from: location)
  let unzippedFileLocation = try unzipFile(at: movedFileLocation)
  MyNotificationCenter.value!.post()
}

private func moveFile(from location: URL) throws -> URL { fatalError() }
private func unzipFile(at location: URL) throws -> URL { fatalError() }

actor MyActor {
  func test() {
    var session: MySession?
    defer { session?.end() }
  }
}

extension MyAsset {
  func continuationResultTiedToContinuation(withStringEnum stringEnum: MyStringEnum) async throws -> sending [MyAssetTrack] {
    try await loadTracks(withStringEnum: stringEnum)
  }
}
