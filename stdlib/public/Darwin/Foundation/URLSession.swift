//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

@_exported import Foundation // Clang module

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension URLSessionWebSocketTask {
    public enum Message {
        case data(Data)
        case string(String)
    }

    public func send(_ message: Message, completionHandler: @escaping (Error?) -> Void) {
        switch message {
        case .data(let data):
            __send(__NSURLSessionWebSocketMessage(data: data), completionHandler: completionHandler)
        case .string(let string):
            __send(__NSURLSessionWebSocketMessage(string: string), completionHandler: completionHandler)
        }
    }

    public func receive(completionHandler: @escaping (Result<Message, Error>) -> Void) {
        __receiveMessage { message, error in
            switch (message, error) {
            case (.some(let message), nil):
                switch message.type {
                case .data:
                    completionHandler(.success(.data(message.data!)))
                case .string:
                    completionHandler(.success(.string(message.string!)))
                @unknown default:
                    break
                }
            case (nil, .some(let error)):
                completionHandler(.failure(error))
            case (_, _):
                fatalError("Only one of message or error should be nil")
            }
        }
    }
}

@available(macOS 10.15, iOS 13.0, watchOS 6.0, tvOS 13.0, *)
extension URLSessionTaskTransactionMetrics {
  public var localPort: Int? {
    return __localPort as? Int
  }

  public var remotePort: Int? {
    return __remotePort as? Int
  }

  public var negotiatedTLSProtocolVersion: tls_protocol_version_t? {
    return (__negotiatedTLSProtocolVersion as? UInt16).flatMap(tls_protocol_version_t.init(rawValue:))
  }

  public var negotiatedTLSCipherSuite: tls_ciphersuite_t? {
    return (__negotiatedTLSCipherSuite as? UInt16).flatMap(tls_ciphersuite_t.init(rawValue:))
  }
}
