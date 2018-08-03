//===--------------------- SourceKitdClient.swift -------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// This file provides a wrapper of SourceKitd service.
//===----------------------------------------------------------------------===//

import sourcekitd

public class SourceKitdService {

  public init() {
    sourcekitd_initialize()
  }
  deinit {
    sourcekitd_shutdown()
  }

  /// Send a request synchronously with a handler for its response.
  /// - Parameter request: The request to send.
  /// - Returns: The response from the sourcekitd service.
  public func sendSyn(request: SourceKitdRequest) -> SourceKitdResponse {
    return SourceKitdResponse(resp: sourcekitd_send_request_sync(request.rawRequest))
  }

  /// Send a request asynchronously with a handler for its response.
  /// - Parameter request: The request to send.
  /// - Parameter handler: The handler for the response in the future.
  public func send(request: SourceKitdRequest,
                   handler: @escaping (SourceKitdResponse) -> ())  {
    sourcekitd_send_request(request.rawRequest, nil) { response in
      guard let response = response else { return }
      handler(SourceKitdResponse(resp: response))
    }
  }
}
