//===--- NIOChannelPipeline.swift -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

// Mini benchmark implementing the gist of SwiftNIO's ChannelPipeline as
// implemented by NIO 1 and NIO 2.[01]
let t: [BenchmarkCategory] = [.runtime, .refcount, .cpubench]
let N = 100

public let NIOChannelPipeline = [
  BenchmarkInfo(
    name: "NIOChannelPipeline",
    runFunction: runBench,
    tags: t),
]

public protocol EventHandler: class {
    func event(holder: Holder)
}

extension EventHandler {
    public func event(holder: Holder) {
        holder.fireEvent()
    }
}

public final class Pipeline {
    var head: Holder? = nil

    public init() {}

    public func addHandler(_ handler: EventHandler) {
        if self.head == nil {
            self.head = Holder(handler)
            return
        }

        var node = self.head
        while node?.next != nil {
            node = node?.next
        }
        node?.next = Holder(handler)
    }

    public func fireEvent() {
        self.head!.invokeEvent()
    }
}

public final class Holder {
    var next: Holder?
    let node: EventHandler

    init(_ node: EventHandler) {
        self.next = nil
        self.node = node
    }

    func invokeEvent() {
        self.node.event(holder: self)
    }

    @inline(never)
    public func fireEvent() {
        self.next?.invokeEvent()
    }
}

public final class NoOpHandler: EventHandler {
    public init() {}
}

public final class ConsumingHandler: EventHandler {
    var consumed = 0
    public init() {}
    public func event(holder: Holder) {
        self.consumed += 1
    }
}

@inline(never)
func runBench(iterations: Int) {
    let pipeline = Pipeline()
    for _ in 0..<5 {
        pipeline.addHandler(NoOpHandler())
    }
    pipeline.addHandler(ConsumingHandler())

    for _ in 0 ..< iterations {
        for _ in 0 ..< 1000 {
            pipeline.fireEvent()
        }
    }
}
