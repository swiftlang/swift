// RUN: %target-swift-frontend -parse-as-library -emit-sil -verify %s

// REQUIRES: concurrency

// NOTE: Once `-swift-version 6` exists, you should update this test so that
// most of its warnings are expected to be errors. This test intentionally does
// not specify a Swift version, so that once Swift 6 exists, you'll see this
// test break and update it accordingly. -kavon

enum BogusError: Error {
    case blah
}

@available(SwiftStdlib 5.1, *)
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
        say(msg: "hello?")

        Task { self }
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

@available(SwiftStdlib 5.1, *)
func callMethod(_ a: MyActor) {}

@available(SwiftStdlib 5.1, *)
func passInout<T>(_ a: inout T) {}

@available(SwiftStdlib 5.1, *)
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

        Task { self } // expected-note 2 {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}

        self.x = randomInt()  // expected-warning {{cannot access property 'x' here in non-isolated initializer}}

        callMethod(self) // expected-note 5 {{after calling global function 'callMethod', only non-isolated properties of 'self' can be accessed from this init}}

        passInout(&self.x) // expected-warning {{cannot access property 'x' here in non-isolated initializer}}

        if c {
          // expected-warning@+2 {{cannot access property 'y' here in non-isolated initializer}}
          // expected-warning@+1 {{cannot access property 'x' here in non-isolated initializer}}
          self.x = self.y
        }

        // expected-warning@+2 {{cannot access property 'y' here in non-isolated initializer}}
        // expected-warning@+1 {{cannot access property 'x' here in non-isolated initializer}}
        (_, _) = (self.x, self.y)
        _ = self.x == 0 // expected-warning {{cannot access property 'x' here in non-isolated initializer}}

        while c {
          // expected-warning@+2 {{cannot access property 'hax' here in non-isolated initializer}}
          // expected-note@+1 2 {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}
          self.hax = self
          _ = self.hax  // expected-warning {{cannot access property 'hax' here in non-isolated initializer}}
        }

        Task {
            _ = await self.hax
            await self.helloWorld()
        }

        { _ = self }()
    }

    @MainActor
    init(i2 c:  Bool) {
        self.x = 0
        self.y = self.x

        Task { self } // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}

        callMethod(self)

        passInout(&self.x) // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
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
            _ = self.hax
            self.helloWorld()
        }

      { _ = self }()
    }

    @MainActor
    init(i4 c:  Bool) async {
      self.x = 0
      self.y = self.x

      Task { self } // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}

      callMethod(self)

      passInout(&self.x) // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
    }
}


@available(SwiftStdlib 5.1, *)
actor X {
    var counter: Int

    init(v1 start: Int) {
        self.counter = start
        Task { await self.setCounter(start + 1) } // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}

        if self.counter != start { // expected-warning {{cannot access property 'counter' here in non-isolated initializer}}
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


@available(SwiftStdlib 5.1, *)
var globalVar: EscapeArtist?

@available(SwiftStdlib 5.1, *)
actor EscapeArtist {
    var x: Int

    init(attempt1: Bool) {
        self.x = 0

        globalVar = self    // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}
        Task { await globalVar!.isolatedMethod() }

        if self.x == 0 {  // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
            fatalError("race detected.")
        }
    }

    init(attempt2: Bool) {
        self.x = 0

        let wrapped: EscapeArtist? = .some(self)    // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}
        let selfUnchained = wrapped!

        Task { await selfUnchained.isolatedMethod() }
        if self.x == 0 {  // expected-warning {{cannot access property 'x' here in non-isolated initializer}}
            fatalError("race detected.")
        }
    }

    init(attempt3: Bool) {
        self.x = 0

        let unchainedSelf = self

        unchainedSelf.nonisolated()
    }

    init(attempt4: Bool) {
        self.x = 0

        let unchainedSelf = self

        unchainedSelf.nonisolated()

        let _ = { unchainedSelf.nonisolated() }()
    }

    init(attempt5: Bool) {
        self.x = 0

        let box = CardboardBox(item: self)
        box.item.nonisolated()
    }

    init(attempt6: Bool) {
        self.x = 0
        func fn() {
            self.nonisolated()
        }
        fn()
    }

    func isolatedMethod() { x += 1 }
    nonisolated func nonisolated() {}
}

@available(SwiftStdlib 5.5, *)
actor Ahmad {
  nonisolated func f() {}
  var prop: Int = 0
  
  init(v1: Void) {
    Task.detached { self.f() } // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}
    f()
    prop += 1 // expected-warning {{cannot access property 'prop' here in non-isolated initializer}}
  }
  
  nonisolated init(v2: Void) async {
    Task.detached { self.f() } // expected-note {{after making a copy of 'self', only non-isolated properties of 'self' can be accessed from this init}}
    f()
    prop += 1 // expected-warning {{cannot access property 'prop' here in non-isolated initializer}}
  }
}

@available(SwiftStdlib 5.5, *)
actor Rain {
  var x: Int = 0
  nonisolated func f() {}

  init(_ hollerBack: (Rain) -> () -> Void) {
    defer { self.f() }

    defer { _ = self.x }  // expected-warning {{cannot access property 'x' here in non-isolated initializer}}

    defer { Task { self.f() } }

    defer { _ = hollerBack(self) } // expected-note {{after a call involving 'self', only non-isolated properties of 'self' can be accessed from this init}}
  }

  init(_ hollerBack: (Rain) -> () -> Void) async {
    defer { self.f() }

    defer { _ = self.x }

    defer { Task { self.f() } }

    defer { _ = hollerBack(self) }
  }
}
