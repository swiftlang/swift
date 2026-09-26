// A miscompilation, not a fix: the CHECK lines below describe the correct
// ordering, and the optimizer does not currently produce it. See the XFAIL.
//
// In an `async` function, extracting `String` fields out of a decoded payload
// and returning them in a new value emits `destroy_addr` of the payload BEFORE
// the `strong_retain` of the extracted bridge objects, so the returned `String`s
// are already released when the function returns. Requires the payload struct to
// carry a trailing stored property of a resilient type; with a fragile type of
// the same layout the retains are correctly ordered before the destroy.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Build the resilient library separately.
// RUN: %target-swift-frontend -emit-module -parse-as-library -O \
// RUN:   -enable-library-evolution -disable-availability-checking \
// RUN:   -module-name Stamp %t/library.swift \
// RUN:   -emit-module-path %t/Stamp.swiftmodule

// RUN: %target-swift-frontend -emit-sil -O -I %t \
// RUN:   -disable-availability-checking \
// RUN:   -module-name main %t/client.swift | %FileCheck %s

// XFAIL: *

// REQUIRES: concurrency

//--- library.swift

public struct Stamp: Sendable, Hashable {
    public var v: Double
    public init(v: Double) { self.v = v }
}

//--- client.swift

import Stamp

public struct Decoded: Sendable, Hashable {
    public var nonce: String
    public var continuation: String
    // Remove this field, or give it a fragile type such as `Double` or a struct
    // declared in this module, and the miscompile below disappears.
    public var expiresAt: Stamp
    public init(nonce: String, continuation: String, expiresAt: Stamp) {
        self.nonce = nonce
        self.continuation = continuation
        self.expiresAt = expiresAt
    }
}

public struct Refusal: Sendable, Hashable {
    public var reason: String
    public init(reason: String) { self.reason = reason }
}

public enum Output: Sendable, Hashable {
    public struct Ok: Sendable, Hashable {
        public enum Body: Sendable, Hashable {
            case json(Decoded)
            public var json: Decoded {
                get throws { switch self { case let .json(b): return b } }
            }
        }
        public var body: Body
        public init(body: Body) { self.body = body }
    }
    public struct Unprocessable: Sendable, Hashable {
        public enum Body: Sendable, Hashable {
            case json(Refusal)
            public var json: Refusal {
                get throws { switch self { case let .json(b): return b } }
            }
        }
        public var body: Body
        public init(body: Body) { self.body = body }
    }
    case ok(Ok)
    case unprocessable(Unprocessable)
    case undocumented(statusCode: Int)
}

public protocol Transport: Sendable {
    func send(_ p: String) async throws -> Output
}

public struct Intent: Sendable, Equatable {
    public let nonce: String
    public let continuation: String
    public init(nonce: String, continuation: String) {
        self.nonce = nonce
        self.continuation = continuation
    }
}

public enum IntentRead: Sendable, Equatable {
    case ok(Intent)
    case refused(String?)
    case unavailable
}

public struct API: Sendable {
    let transport: any Transport
    public init(transport: any Transport) { self.transport = transport }

    // `body` is an owned value and reading `body.nonce` is a copy, so both
    // extracted `String`s must be retained before the payload is destroyed.
    //
    // CHECK-LABEL: sil {{.*}}@$s4main3APIV10makeIntent8providerAA0D4ReadOSS_tYaF :
    // CHECK-NOT: destroy_addr
    // CHECK: strong_retain
    // CHECK-NOT: destroy_addr
    // CHECK: strong_retain
    public func makeIntent(provider: String) async -> IntentRead {
        do {
            switch try await transport.send(provider) {
            case .ok(let ok):
                let body = try ok.body.json
                guard !body.nonce.isEmpty, !body.continuation.isEmpty else { return .unavailable }
                return .ok(.init(nonce: body.nonce, continuation: body.continuation))
            case .unprocessable(let refusal):
                return .refused(try refusal.body.json.reason)
            case .undocumented:
                return .unavailable
            }
        } catch {
            return .unavailable
        }
    }
}
