// RUN: %target-typecheck-verify-swift

func test1(@_inheritActorContext _: @Sendable () async -> Void) {} // Ok
func test2(@_inheritActorContext(always) _: sending () async -> Void) {} // Ok

func test3(@_inheritActorContext _: () -> Void) {}

func test4(@_inheritActorContext _: Int) {}
// expected-error@-1 {{@_inheritActorContext only applies to parameters with function types (got: 'Int')}}

func test5(@_inheritActorContext _: Optional<() async -> Int>) {} // Ok
func test6(@_inheritActorContext _: (Optional<() async -> Int>)?) {} // Ok

func test7(@_inheritActorContext _: Int?) {} // Ok
// expected-error@-1 {{@_inheritActorContext only applies to parameters with function types (got: 'Int?')}}

struct S {
  init(@_inheritActorContext(always) _: @escaping @Sendable () async -> Void) {} // Ok

  var test: @_inheritActorContext () async -> Void { // expected-error {{attribute can only be applied to declarations, not types}}
    { }
  }

  subscript(@_inheritActorContext _: @Sendable () async -> Void) -> Bool { false } // Ok
  subscript(@_inheritActorContext(always) _: @Sendable (Int) async -> Void) -> Bool { false } // Ok

  subscript(@_inheritActorContext _: (String) -> Void) -> Bool { false }
  subscript(x: Int, @_inheritActorContext(always) _: (Int, Int) -> Void) -> Bool { false }

  func testClosure() {
    _ = { @_inheritActorContext in // expected-error {{attribute @_inheritActorContext is not supported on a closure}}
    }
  }
}

