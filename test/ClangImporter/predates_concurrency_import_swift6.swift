// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/Inputs/custom-modules %s -verify -swift-version 6

// REQUIRES: objc_interop
// REQUIRES: concurrency

@preconcurrency import Foundation

func acceptSendable<T: Sendable>(_: T) { }

func useSendable(ns: NSString) {
  // Note: warning below is suppressed due to @preconcurrency
  acceptSendable(ns)
}
