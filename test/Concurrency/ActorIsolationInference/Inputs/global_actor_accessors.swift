actor SomeActor {}

@globalActor
struct GA1 {
  static let shared = SomeActor()
}
@globalActor
struct GA2 {
  static let shared = SomeActor()
}

// expected-swift6-note@+1 * {{calls to global function 'ga2()' from outside of its actor context are implicitly asynchronous}}
@GA2 func ga2() {}

// Global actor on global var accessors.

@GA1 var globalObserved: Int = 0 {
  // expected-swift6-error@+2 {{cannot have a global actor}}
  // expected-swift5-warning@+1 {{cannot have a global actor}}
  @GA2 willSet {
    ga2()
    // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
  }
  // expected-swift6-error@+2 {{cannot have a global actor}}
  // expected-swift5-warning@+1 {{cannot have a global actor}}
  @GA2 didSet {
    ga2()
    // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
  }
}

@GA1 var globalComputed: Int {
  // expected-swift6-note@+3 {{global actor attribute is ignored}}
  // expected-swift6-warning@+2 {{cannot have a global actor}}
  // expected-swift5-warning@+1 {{cannot have a global actor}}
  @GA2 get {
    ga2()
    // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}

    return 0
  }
  // expected-swift6-error@+2 {{cannot have a global actor}}
  // expected-swift5-warning@+1 {{cannot have a global actor}}
  @GA2 set {
    ga2()
    // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
  }
  // expected-swift6-error@+2 {{cannot have a global actor}}
  // expected-swift5-warning@+1 {{cannot have a global actor}}
  @GA2 _modify {
    ga2()
    // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
  }
}

@GA1 var globalComputedRead: Int {
  // expected-swift6-error@+2 {{cannot have a global actor}}
  // expected-swift5-warning@+1 {{cannot have a global actor}}
  @GA2 _read {
    ga2()
    // expected-swift6-error@+-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
  }
}

@GA1 func testGlobalStorageDeclActorIsolation() {
  let _ = globalObserved
  globalObserved = 0

  let _ = globalComputed
  globalComputed = 0

  let _ = globalComputedRead
}

// Global actor on property/subscript accessors.

@GA1 struct TypeContext {
  nonisolated init() {}

  var observed: Int {
    // expected-swift6-error@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 willSet {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
    }
    // expected-swift6-error@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 didSet {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
    }
  }

  var computed: Int {
    // expected-swift6-note@+4 {{global actor attribute is ignored}}
    // expected-swift6-warning@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 get {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}

      return 0
    }
    // expected-swift6-error@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 set {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
    }
    // expected-swift6-error@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 _modify {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
    }
    // expected-swift6-error@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 init {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
    }
  }

  var computedRead: Int {
    // expected-swift6-error@+3 {{cannot have a global actor}}
    // expected-swift5-warning@+2 {{cannot have a global actor}}
    // expected-note@+1 {{move global actor}}
    @GA2 _read {
      ga2()
      // expected-swift6-error@-1 {{call to global actor 'GA2'-isolated global function 'ga2()' in a synchronous global actor 'GA1'-isolated context}}
    }
  }
}

@GA1 func testMemberStorageDeclActorIsolation() {
  var c = TypeContext()

  let _ = c.observed
  c.observed = 0

  let _ = c.computed
  c.computed = 0

  let _ = c.computedRead
}


