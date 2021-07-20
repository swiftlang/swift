// RUN: %target-swift-frontend -parse-as-library -emit-sil -verify %s

// REQUIRES: concurrency

// NOTE: Once `-swift-version 6` exists, you should update this test so that
// most of its warnings are expected to be errors. This test intentionally does
// not specify a Swift version, so that once Swift 6 exists, you'll see this
// test break and update it accordingly. -kavon

enum BogusError: Error {
    case blah
}

@available(SwiftStdlib 5.5, *)
actor Convenient {
    var x: Int
    var y: Convenient?

    init(val: Int) {
        self.x = val
    }

    convenience init(bigVal: Int) {
        if bigVal < 0 {
            self.init(val: 0)
            say(msg: "hello from actor!")
        }
        say(msg: "said this too early!") // expected-error {{'self' used before 'self.init' call or assignment to 'self'}}
        self.init(val: bigVal)

        Task { await self.mutateIsolatedState() }
    }

    convenience init!(biggerVal1 biggerVal: Int) {
        guard biggerVal < 1234567 else { return nil }
        self.init(bigVal: biggerVal)
        say(msg: "hello?")
    }

    @MainActor
    convenience init?(biggerVal2 biggerVal: Int) async {
        guard biggerVal < 1234567 else { return nil }
        self.init(bigVal: biggerVal)
        say(msg: "hello?")
        await mutateIsolatedState()
    }

    convenience init() async {
        self.init(val: 10)
        await mutateIsolatedState()
    }

    init(throwyDesignated val: Int) throws {
        guard val > 0 else { throw BogusError.blah }
        self.x = 10
        say(msg: "hello?")  // expected-warning {{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        Task { self }       // expected-warning {{actor 'self' can only be captured by a closure from an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
    }

    init(asyncThrowyDesignated val: Int) async throws {
        guard val > 0 else { throw BogusError.blah }
        self.x = 10
        say(msg: "hello?")
        Task { self }
    }

    convenience init(throwyConvenient val: Int) throws {
        try self.init(throwyDesignated: val)
        say(msg: "hello?")
        Task { self }
    }

    func mutateIsolatedState() {
        self.y = self
    }

    nonisolated func say(msg: String) {
        print(msg)
    }
}

func randomInt() -> Int { return 4 }

@available(SwiftStdlib 5.5, *)
func callMethod(_ a: MyActor) {}

@available(SwiftStdlib 5.5, *)
func passInout<T>(_ a: inout T) {}

@available(SwiftStdlib 5.5, *)
actor MyActor {
    var x: Int
    var y: Int
    var hax: MyActor?

    var computedProp : Int {
        get { 0 }
        set { }
    }

    func helloWorld() {}

    convenience init(ci1 c: Bool) {
        self.init(i1: c)
        Task { self }
        callMethod(self)
    }

    init(i1 c:  Bool) {
        self.x = 0
        _ = self.x
        self.y = self.x

        Task { self }     // expected-warning{{actor 'self' can only be captured by a closure from an async initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        self.helloWorld() // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        callMethod(self) // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                         // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        passInout(&self.x) // expected-warning{{actor 'self' can only be passed 'inout' from an async initializer}}

        self.x = self.y
        self.x = randomInt()
        (_, _) = (self.x, self.y)
        _ = self.x == 0

        self.hax = self     // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
        _ = self.hax

        _ = computedProp    // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        computedProp = 1    // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        Task { // expected-warning {{actor 'self' can only be captured by a closure from an async initializer}}
               // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
            _ = await self.hax
            await self.helloWorld()
        }
    }

    init?(i1_nil c:  Bool) {
        self.x = 0
        guard c else { return nil }
        self.y = self.x

        Task { self }     // expected-warning{{actor 'self' can only be captured by a closure from an async initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        self.helloWorld() // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        callMethod(self) // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                         // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        passInout(&self.x) // expected-warning{{actor 'self' can only be passed 'inout' from an async initializer}}

        self.x = self.y

        self.hax = self     // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
        _ = self.hax

        _ = computedProp    // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        computedProp = 1    // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        Task { // expected-warning {{actor 'self' can only be captured by a closure from an async initializer}}
               // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
            _ = await self.hax
            await self.helloWorld()
        }
    }

    init!(i1_boom c:  Bool) {
        self.x = 0
        guard c else { return nil }
        self.y = self.x

        Task { self }     // expected-warning{{actor 'self' can only be captured by a closure from an async initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        self.helloWorld() // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        callMethod(self) // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                         // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        passInout(&self.x) // expected-warning{{actor 'self' can only be passed 'inout' from an async initializer}}

        self.x = self.y

        self.hax = self     // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
        _ = self.hax

        _ = computedProp    // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        computedProp = 1    // expected-warning{{this use of actor 'self' can only appear in an async initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        Task { // expected-warning {{actor 'self' can only be captured by a closure from an async initializer}}
               // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
            _ = await self.hax
            await self.helloWorld()
        }
    }

    @MainActor
    init(i2 c:  Bool) {
        self.x = 0
        self.y = self.x

        Task { self }     // expected-warning{{actor 'self' cannot be captured by a closure from a global-actor isolated initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        self.helloWorld() // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        callMethod(self) // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                         // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        passInout(&self.x) // expected-warning{{actor 'self' cannot be passed 'inout' from a global-actor isolated initializer}}

        self.x = self.y

        self.hax = self     // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
        _ = self.hax

        _ = computedProp    // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        computedProp = 1    // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        Task { // expected-warning {{actor 'self' cannot be captured by a closure from a global-actor isolated initializer}}
               // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
            _ = await self.hax
            await self.helloWorld()
        }
    }

    init(i3 c:  Bool) async {
        self.x = 0
        self.y = self.x

        Task { self }

        self.helloWorld()
        callMethod(self)
        passInout(&self.x)

        self.x = self.y

        self.hax = self
        _ = Optional.some(self)

        _ = computedProp
        computedProp = 1

        Task {
            _ = await self.hax
            await self.helloWorld()
        }
    }

    @MainActor
    init(i4 c:  Bool) async {
        self.x = 0
        self.y = self.x

        Task { self }     // expected-warning{{actor 'self' cannot be captured by a closure from a global-actor isolated initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        self.helloWorld() // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                          // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        callMethod(self) // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                         // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        passInout(&self.x) // expected-warning{{actor 'self' cannot be passed 'inout' from a global-actor isolated initializer}}

        self.x = self.y

        self.hax = self     // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
        _ = self.hax

        _ = computedProp    // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        computedProp = 1    // expected-warning{{this use of actor 'self' cannot appear in a global-actor isolated initializer}}
                            // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        Task { // expected-warning {{actor 'self' cannot be captured by a closure from a global-actor isolated initializer}}
               // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
            _ = await self.hax
            await self.helloWorld()
        }
    }

}


@available(SwiftStdlib 5.5, *)
actor X {
    var counter: Int

    init(v1 start: Int) {
        self.counter = start
        Task { await self.setCounter(start + 1) } // expected-warning {{actor 'self' can only be captured by a closure from an async initializer}}
                                                  // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        if self.counter != start {
            fatalError("where's my protection?")
        }
    }

    func setCounter(_ x : Int) {
        self.counter = x
    }
}

struct CardboardBox<T> {
    public let item: T
}


@available(SwiftStdlib 5.5, *)
var globalVar: EscapeArtist?

@available(SwiftStdlib 5.5, *)
actor EscapeArtist {
    var x: Int

    init(attempt1: Bool) {
        self.x = 0

        globalVar = self    // expected-warning {{this use of actor 'self' can only appear in an async initializer}}
        Task { await globalVar!.isolatedMethod() }

        if self.x == 0 {
            fatalError("race detected.")
        }
    }

    init(attempt2: Bool) {
        self.x = 0

        let wrapped: EscapeArtist? = .some(self)    // expected-warning {{this use of actor 'self' can only appear in an async initializer}}
        let selfUnchained = wrapped!

        Task { await selfUnchained.isolatedMethod() }
        if self.x == 0 {
            fatalError("race detected.")
        }
    }

    init(attempt3: Bool) {
        self.x = 0

        // expected-warning@+2 {{variable 'unchainedSelf' was never mutated; consider changing to 'let' constant}}
        // expected-warning@+1 {{this use of actor 'self' can only appear in an async initializer}}
        var unchainedSelf = self

        unchainedSelf.nonisolated()
    }

    init(attempt4: Bool) {
        self.x = 0

        let unchainedSelf = self

        unchainedSelf.nonisolated() // expected-warning {{this use of actor 'self' can only appear in an async initializer}}
                                    // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}

        let _ = { unchainedSelf.nonisolated() } // expected-warning {{actor 'self' can only be captured by a closure from an async initializer}}
                                                // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
    }

    init(attempt5: Bool) {
        self.x = 0

        let box = CardboardBox(item: self) // expected-warning {{this use of actor 'self' can only appear in an async initializer}}
        box.item.nonisolated()
    }

    init(attempt6: Bool) {
        self.x = 0
        func fn() {
            self.nonisolated()
        }
        fn()    // expected-warning {{this use of actor 'self' can only appear in an async initializer}}
                // expected-note@-1 {{convenience initializers allow non-isolated use of 'self' once initialized}}
    }

    func isolatedMethod() { x += 1 }
    nonisolated func nonisolated() {}
}