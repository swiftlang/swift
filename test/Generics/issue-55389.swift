// RUN: %target-typecheck-verify-swift

struct Composite<Foo> { }

protocol Chain {
  associatedtype Next: Chain
  associatedtype Foo = Composite<Next.Foo>
}

// expected-error@+3 {{cannot build rewrite system for protocol; rule length limit exceeded}}
// expected-note@+2 {{failed rewrite rule}}
// expected-error@+1 {{'Foo' is not a member type of type 'Self.Next'}}
protocol OtherChain where Foo == Composite<Next.Foo> {
  associatedtype Next: OtherChain
  associatedtype Foo
}
