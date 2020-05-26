// RUN: %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
// REQUIRES: foundation

import StdlibUnittest
import Foundation

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
private func testWebSocketTask() {
  let task = URLSession.shared.webSocketTask(with: URL(string:"wss://test.example")!)

  task.resume()

  task.send(.string("Hello")) { error in
    assert(error == nil)
  }

  task.receive { result in
    switch result {
    case .success(.string(let string)):
      assert(string == "Hello")
      task.cancel()
    default:
      assertionFailure()
    }
  }
}

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
private func testURLError(_ error: Error) {
  if let error = error as? URLError {
    if error.networkUnavailableReason == .constrained {
      // Handle Low Data Mode
    }
    if error.backgroundTaskCancelledReason == .backgroundUpdatesDisabled {
      // Background refresh disabled
    }
    _ = try? error.downloadTaskResumeData?.write(to: URL(fileURLWithPath: "/tmp/1.data"))
  }
}

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
private func testURLCache() {
  _ = URLCache(memoryCapacity: 0, diskCapacity: 0)
  _ = URLCache(memoryCapacity: 0, diskCapacity: 0, directory: URL(fileURLWithPath: "/tmp"))
}

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
private func testTaskMetrics(_ metrics: URLSessionTaskMetrics) {
  if let transaction = metrics.transactionMetrics.last {
    if transaction.remotePort == 443 {
      // HTTPS default
    }
    if transaction.negotiatedTLSProtocolVersion == .TLSv13 {
      // TLS 1.3
    }
    if transaction.negotiatedTLSCipherSuite == .CHACHA20_POLY1305_SHA256 {
      // CHACHA20_POLY1305_SHA256
    }
  }
}
