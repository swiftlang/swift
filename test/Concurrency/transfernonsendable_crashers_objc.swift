// RUN: %target-swift-frontend -emit-sil -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -target %target-swift-5.1-abi-triple %s -o /dev/null -import-objc-header %S/Inputs/transfernonsendable_crashers_objc.h -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: objc_interop
// REQUIRES: asserts

import Foundation

extension MyNotificationCenter {
  static var value = MyNotificationCenter()
}

public func handleFile(at location: URL) throws {
//  createDownloadDirectoryIfRequired()
  let movedFileLocation = try moveTheme(from: location)
  let unzippedFileLocation = try unzipFile(at: movedFileLocation)
  MyNotificationCenter.value!.post()
}

private func moveTheme(from location: URL) throws -> URL { fatalError() }
private func unzipFile(at location: URL) throws -> URL { fatalError() }

actor MyActor {
  func test() {
    var session: MySession?
    defer { session?.end() }
  }
}
