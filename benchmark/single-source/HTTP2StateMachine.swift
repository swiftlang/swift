//===--- HTTP2StateMachine.swift ------------------------------------------===//
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

import TestsUtils

//
// A trimmed-down version of SwiftNIO's HTTP/2 stream state machine. This version removes all code
// comments and removes any custom data types that are used by SwiftNIO. Its purpose is to benchmark
// Swift's performance switching over enums with substantial amounts of associated data.
//

public let benchmarks = [
  BenchmarkInfo(
    name: "HTTP2StateMachine",
    runFunction: run_HTTP2StateMachine,
    tags: [.miniapplication])
]

typealias HTTP2FlowControlWindow = Int


struct HTTP2StreamStateMachine {
    enum State {
        case idle(localRole: StreamRole, localWindow: HTTP2FlowControlWindow, remoteWindow: HTTP2FlowControlWindow)
        case reservedRemote(remoteWindow: HTTP2FlowControlWindow)
        case reservedLocal(localWindow: HTTP2FlowControlWindow)
        case halfOpenLocalPeerIdle(localWindow: HTTP2FlowControlWindow, remoteWindow: HTTP2FlowControlWindow)
        case halfOpenRemoteLocalIdle(localWindow: HTTP2FlowControlWindow, remoteWindow: HTTP2FlowControlWindow)
        case fullyOpen(localRole: StreamRole, localWindow: HTTP2FlowControlWindow, remoteWindow: HTTP2FlowControlWindow)
        case halfClosedLocalPeerIdle(remoteWindow: HTTP2FlowControlWindow)
        case halfClosedLocalPeerActive(localRole: StreamRole, initiatedBy: StreamRole, remoteWindow: HTTP2FlowControlWindow)
        case halfClosedRemoteLocalIdle(localWindow: HTTP2FlowControlWindow)
        case halfClosedRemoteLocalActive(localRole: StreamRole, initiatedBy: StreamRole, localWindow: HTTP2FlowControlWindow)
        case closed
    }

    enum StreamRole {
        case server
        case client
    }

    private var state: State

    init(localRole: StreamRole, localWindow: HTTP2FlowControlWindow, remoteWindow: HTTP2FlowControlWindow) {
        self.state = .idle(localRole: localRole, localWindow: localWindow, remoteWindow: remoteWindow)
    }

    init(receivedPushPromiseWithRemoteInitialWindowSize remoteWindow: HTTP2FlowControlWindow) {
        self.state = .reservedRemote(remoteWindow: remoteWindow)
    }

    init(sentPushPromiseWithLocalInitialWindowSize localWindow: HTTP2FlowControlWindow) {
        self.state = .reservedLocal(localWindow: localWindow)
    }

    @inline(never)
    mutating func sendHeaders(isEndStreamSet endStream: Bool) -> Bool {
        switch self.state {
        case .idle(.client, localWindow: let localWindow, remoteWindow: let remoteWindow):
            self.state = endStream ? .halfClosedLocalPeerIdle(remoteWindow: remoteWindow) : .halfOpenLocalPeerIdle(localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .halfOpenRemoteLocalIdle(localWindow: let localWindow, remoteWindow: let remoteWindow):
            self.state = endStream ? .halfClosedLocalPeerActive(localRole: .server, initiatedBy: .client, remoteWindow: remoteWindow) : .fullyOpen(localRole: .server, localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .halfOpenLocalPeerIdle(localWindow: _, remoteWindow: let remoteWindow):
            self.state = .halfClosedLocalPeerIdle(remoteWindow: remoteWindow)
            return true

        case .reservedLocal(let localWindow):
            self.state = endStream ? .closed : .halfClosedRemoteLocalActive(localRole: .server, initiatedBy: .server, localWindow: localWindow)
            return true

        case .fullyOpen(let localRole, localWindow: _, remoteWindow: let remoteWindow):
            self.state = .halfClosedLocalPeerActive(localRole: localRole, initiatedBy: .client, remoteWindow: remoteWindow)
            return true

        case .halfClosedRemoteLocalIdle(let localWindow):
            self.state = endStream ? .closed : . halfClosedRemoteLocalActive(localRole: .server, initiatedBy: .client, localWindow: localWindow)
            return true

        case .halfClosedRemoteLocalActive:
            self.state = .closed
            return true


        case .idle(.server, _, _), .closed:
            return false

        case .reservedRemote, .halfClosedLocalPeerIdle, .halfClosedLocalPeerActive:
            return false
        }
    }

    @inline(never)
    mutating func receiveHeaders(isEndStreamSet endStream: Bool) -> Bool {
        switch self.state {
        case .idle(.server, localWindow: let localWindow, remoteWindow: let remoteWindow):
            self.state = endStream ? .halfClosedRemoteLocalIdle(localWindow: localWindow) : .halfOpenRemoteLocalIdle(localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .halfOpenLocalPeerIdle(localWindow: let localWindow, remoteWindow: let remoteWindow):
            self.state = endStream ? .halfClosedRemoteLocalActive(localRole: .client,initiatedBy: .client, localWindow: localWindow) : .fullyOpen(localRole: .client, localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .halfOpenRemoteLocalIdle(localWindow: let localWindow, remoteWindow: _):
            self.state = .halfClosedRemoteLocalIdle(localWindow: localWindow)
            return true

        case .reservedRemote(let remoteWindow):
            self.state = endStream ? .closed : .halfClosedLocalPeerActive(localRole: .client, initiatedBy: .server, remoteWindow: remoteWindow)
            return true

        case .fullyOpen(let localRole, localWindow: let localWindow, remoteWindow: _):
            self.state = .halfClosedRemoteLocalActive(localRole: localRole, initiatedBy: .client, localWindow: localWindow)
            return true

        case .halfClosedLocalPeerIdle(let remoteWindow):
            self.state = endStream ? .closed : . halfClosedLocalPeerActive(localRole: .client, initiatedBy: .client, remoteWindow: remoteWindow)
            return true

        case .halfClosedLocalPeerActive:
            self.state = .closed
            return true

        case .idle(.client, _, _), .closed:
            return false

        case .reservedLocal, .halfClosedRemoteLocalIdle, .halfClosedRemoteLocalActive:
            return false
        }
    }

    @inline(never)
    mutating func sendData(flowControlledBytes: Int, isEndStreamSet endStream: Bool) -> Bool {
        switch self.state {
        case .halfOpenLocalPeerIdle(localWindow: var localWindow, remoteWindow: let remoteWindow):
            localWindow -= flowControlledBytes
            self.state = endStream ? .halfClosedLocalPeerIdle(remoteWindow: remoteWindow) : .halfOpenLocalPeerIdle(localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .fullyOpen(let localRole, localWindow: var localWindow, remoteWindow: let remoteWindow):
            localWindow -= flowControlledBytes
            self.state = endStream ? .halfClosedLocalPeerActive(localRole: localRole, initiatedBy: .client, remoteWindow: remoteWindow) : .fullyOpen(localRole: localRole, localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .halfClosedRemoteLocalActive(let localRole, let initiatedBy, var localWindow):
            localWindow -= flowControlledBytes
            self.state = endStream ? .closed : .halfClosedRemoteLocalActive(localRole: localRole, initiatedBy: initiatedBy, localWindow: localWindow)
            return true

        case .idle, .halfOpenRemoteLocalIdle, .reservedLocal, .reservedRemote, .halfClosedLocalPeerIdle,
             .halfClosedLocalPeerActive, .halfClosedRemoteLocalIdle, .closed:
            return false
        }
    }

    @inline(never)
    mutating func receiveData(flowControlledBytes: Int, isEndStreamSet endStream: Bool) -> Bool {
        switch self.state {
        case .halfOpenRemoteLocalIdle(localWindow: let localWindow, remoteWindow: var remoteWindow):
            remoteWindow -= flowControlledBytes
            self.state = endStream ? .halfClosedRemoteLocalIdle(localWindow: localWindow) : .halfOpenRemoteLocalIdle(localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .fullyOpen(let localRole, localWindow: let localWindow, remoteWindow: var remoteWindow):
            remoteWindow -= flowControlledBytes
            self.state = endStream ? .halfClosedRemoteLocalActive(localRole: localRole, initiatedBy: .client, localWindow: localWindow) : .fullyOpen(localRole: localRole, localWindow: localWindow, remoteWindow: remoteWindow)
            return true

        case .halfClosedLocalPeerActive(let localRole, let initiatedBy, var remoteWindow):
            remoteWindow -= flowControlledBytes
            self.state = endStream ? .closed : .halfClosedLocalPeerActive(localRole: localRole, initiatedBy: initiatedBy, remoteWindow: remoteWindow)
            return true

        case .idle, .halfOpenLocalPeerIdle, .reservedLocal, .reservedRemote, .halfClosedLocalPeerIdle,
             .halfClosedRemoteLocalActive, .halfClosedRemoteLocalIdle, .closed:
            return false
        }
    }

    @inline(never)
    mutating func sendPushPromise() -> Bool {
        switch self.state {
        case .fullyOpen(localRole: .server, localWindow: _, remoteWindow: _),
             .halfClosedRemoteLocalActive(localRole: .server, initiatedBy: .client, localWindow: _):
            return true

        case .idle, .reservedLocal, .reservedRemote, .halfClosedLocalPeerIdle, .halfClosedLocalPeerActive,
             .halfClosedRemoteLocalIdle, .halfOpenLocalPeerIdle, .halfOpenRemoteLocalIdle, .closed,
             .fullyOpen(localRole: .client, localWindow: _, remoteWindow: _),
             .halfClosedRemoteLocalActive(localRole: .client, initiatedBy: _, localWindow: _),
             .halfClosedRemoteLocalActive(localRole: .server, initiatedBy: .server, localWindow: _):
            return false
        }
    }

    @inline(never)
    mutating func receivePushPromise() -> Bool {
        switch self.state {
        case .fullyOpen(localRole: .client, localWindow: _, remoteWindow: _),
             .halfClosedLocalPeerActive(localRole: .client, initiatedBy: .client, remoteWindow: _):
            return true

        case .idle, .reservedLocal, .reservedRemote, .halfClosedLocalPeerIdle, .halfClosedRemoteLocalIdle,
             .halfClosedRemoteLocalActive, .halfOpenLocalPeerIdle, .halfOpenRemoteLocalIdle, .closed,
             .fullyOpen(localRole: .server, localWindow: _, remoteWindow: _),
             .halfClosedLocalPeerActive(localRole: .server, initiatedBy: _, remoteWindow: _),
             .halfClosedLocalPeerActive(localRole: .client, initiatedBy: .server, remoteWindow: _):
            return false
        }
    }

    @inline(never)
    mutating func sendWindowUpdate(windowIncrement: Int) -> Bool {
        switch self.state {
        case .reservedRemote(remoteWindow: var remoteWindow):
            remoteWindow += windowIncrement
            self.state = .reservedRemote(remoteWindow: remoteWindow)

        case .halfOpenLocalPeerIdle(localWindow: let localWindow, remoteWindow: var remoteWindow):
            remoteWindow += windowIncrement
            self.state = .halfOpenLocalPeerIdle(localWindow: localWindow, remoteWindow: remoteWindow)

        case .halfOpenRemoteLocalIdle(localWindow: let localWindow, remoteWindow: var remoteWindow):
            remoteWindow += windowIncrement
            self.state = .halfOpenRemoteLocalIdle(localWindow: localWindow, remoteWindow: remoteWindow)

        case .fullyOpen(localRole: let localRole, localWindow: let localWindow, remoteWindow: var remoteWindow):
            remoteWindow += windowIncrement
            self.state = .fullyOpen(localRole: localRole, localWindow: localWindow, remoteWindow: remoteWindow)

        case .halfClosedLocalPeerIdle(remoteWindow: var remoteWindow):
            remoteWindow += windowIncrement
            self.state = .halfClosedLocalPeerIdle(remoteWindow: remoteWindow)

        case .halfClosedLocalPeerActive(localRole: let localRole, initiatedBy: let initiatedBy, remoteWindow: var remoteWindow):
            remoteWindow += windowIncrement
            self.state = .halfClosedLocalPeerActive(localRole: localRole, initiatedBy: initiatedBy, remoteWindow: remoteWindow)

        case .idle, .reservedLocal, .halfClosedRemoteLocalIdle, .halfClosedRemoteLocalActive, .closed:
            return false
        }

        return true
    }

    @inline(never)
    mutating func receiveWindowUpdate(windowIncrement: Int) -> Bool {
        switch self.state {
        case .reservedLocal(localWindow: var localWindow):
            localWindow += windowIncrement
            self.state = .reservedLocal(localWindow: localWindow)

        case .halfOpenLocalPeerIdle(localWindow: var localWindow, remoteWindow: let remoteWindow):
            localWindow += windowIncrement
            self.state = .halfOpenLocalPeerIdle(localWindow: localWindow, remoteWindow: remoteWindow)

        case .halfOpenRemoteLocalIdle(localWindow: var localWindow, remoteWindow: let remoteWindow):
            localWindow += windowIncrement
            self.state = .halfOpenRemoteLocalIdle(localWindow: localWindow, remoteWindow: remoteWindow)

        case .fullyOpen(localRole: let localRole, localWindow: var localWindow, remoteWindow: let remoteWindow):
            localWindow += windowIncrement
            self.state = .fullyOpen(localRole: localRole, localWindow: localWindow, remoteWindow: remoteWindow)

        case .halfClosedRemoteLocalIdle(localWindow: var localWindow):
            localWindow += windowIncrement
            self.state = .halfClosedRemoteLocalIdle(localWindow: localWindow)

        case .halfClosedRemoteLocalActive(localRole: let localRole, initiatedBy: let initiatedBy, localWindow: var localWindow):
            localWindow += windowIncrement
            self.state = .halfClosedRemoteLocalActive(localRole: localRole, initiatedBy: initiatedBy, localWindow: localWindow)

        case .halfClosedLocalPeerIdle, .halfClosedLocalPeerActive:
            break

        case .idle, .reservedRemote, .closed:
            return false
        }

        return true
    }
}

@inline(never)
func testSimpleRequestResponse(_ n: Int) -> Bool {
    var successful = true

    var server = HTTP2StreamStateMachine(localRole: .server, localWindow: 1<<16, remoteWindow: n)
    var client = HTTP2StreamStateMachine(localRole: .client, localWindow: 1<<16, remoteWindow: n)

    successful = successful && client.sendHeaders(isEndStreamSet: false)
    successful = successful && server.receiveHeaders(isEndStreamSet: false)

    successful = successful && client.sendData(flowControlledBytes: 128, isEndStreamSet: false)
    successful = successful && client.sendData(flowControlledBytes: 128, isEndStreamSet: false)
    successful = successful && server.receiveData(flowControlledBytes: 128, isEndStreamSet: false)
    successful = successful && server.receiveData(flowControlledBytes: 128, isEndStreamSet: false)

    successful = successful && server.sendWindowUpdate(windowIncrement: 256)
    successful = successful && client.receiveWindowUpdate(windowIncrement: 256)

    successful = successful && client.sendData(flowControlledBytes: 128, isEndStreamSet: true)
    successful = successful && server.receiveData(flowControlledBytes: 128, isEndStreamSet: true)

    successful = successful && server.sendHeaders(isEndStreamSet: false)
    successful = successful && client.receiveHeaders(isEndStreamSet: false)

    successful = successful && server.sendData(flowControlledBytes: 1024, isEndStreamSet: false)
    successful = successful && client.receiveData(flowControlledBytes: 1024, isEndStreamSet: false)
    successful = successful && client.sendWindowUpdate(windowIncrement: 1024)
    successful = successful && server.receiveWindowUpdate(windowIncrement: 1024)

    successful = successful && server.sendData(flowControlledBytes: 1024, isEndStreamSet: true)
    successful = successful && client.receiveData(flowControlledBytes: 1024, isEndStreamSet: true)

    return successful
}

@inline(never)
func testPushedRequests(_ n: Int) -> Bool {
    var successful = true

    var server = HTTP2StreamStateMachine(sentPushPromiseWithLocalInitialWindowSize: n)
    var client = HTTP2StreamStateMachine(receivedPushPromiseWithRemoteInitialWindowSize: n)

    successful = successful && client.sendWindowUpdate(windowIncrement: 1024)

    successful = successful && server.sendHeaders(isEndStreamSet: false)
    successful = successful && client.receiveHeaders(isEndStreamSet: false)

    successful = successful && server.sendData(flowControlledBytes: 1024, isEndStreamSet: false)
    successful = successful && server.sendData(flowControlledBytes: 1024, isEndStreamSet: false)
    successful = successful && client.receiveData(flowControlledBytes: 1024, isEndStreamSet: false)
    successful = successful && client.receiveData(flowControlledBytes: 1024, isEndStreamSet: false)

    successful = successful && client.sendWindowUpdate(windowIncrement: 1024)
    successful = successful && server.receiveWindowUpdate(windowIncrement: 1024)

    successful = successful && server.sendData(flowControlledBytes: 1024, isEndStreamSet: false)
    successful = successful && client.receiveData(flowControlledBytes: 1024, isEndStreamSet: false)

    successful = successful && server.sendHeaders(isEndStreamSet: true)
    successful = successful && client.receiveHeaders(isEndStreamSet: true)

    return successful
}

@inline(never)
func testPushingRequests(_ n: Int) -> Bool {
    var successful = true

    var server = HTTP2StreamStateMachine(localRole: .server, localWindow: 1<<16, remoteWindow: n)
    var client = HTTP2StreamStateMachine(localRole: .client, localWindow: 1<<16, remoteWindow: n)

    successful = successful && client.sendHeaders(isEndStreamSet: true)
    successful = successful && server.receiveHeaders(isEndStreamSet: true)

    successful = successful && server.sendHeaders(isEndStreamSet: false)
    successful = successful && client.receiveHeaders(isEndStreamSet: false)

    successful = successful && server.sendPushPromise()
    successful = successful && client.receivePushPromise()

    successful = successful && server.sendData(flowControlledBytes: 1024, isEndStreamSet: true)
    successful = successful && client.receiveData(flowControlledBytes: 1024, isEndStreamSet: true)

    return successful
}

@inline(never)
func run_HTTP2StateMachine(_ n: Int) {
    for i in 0 ..< 100000 * n {
        check(testSimpleRequestResponse(identity(i)))
        check(testPushedRequests(identity(i)))
        check(testPushingRequests(identity(i)))
    }
}

