// RUN: %target-typecheck-verify-swift

#if hasAttribute(foo)
@foo(this is gibberish)
#endif
struct S1 { }

@frozen
#if hasAttribute(foo)
@foo
#endif
public struct S2 { }

#if hasAttribute(foo)
@foo
#endif
@inlinable
func f1() { }

#if hasAttribute(foo)
@foo
#else
@available(*, deprecated, message: "nope")
@frozen
#endif
public struct S3 { }

// Nested #if
#if hasAttribute(foo)
  @foo
#elseif hasAttribute(available)
  @available(*, deprecated, message: "nope")
#if hasAttribute(frozen)
@frozen
#endif
#endif
public struct S4 { }

// Nested in a type
struct Inner {
#if hasAttribute(foo)
  @foo(this is gibberish)
#endif
  struct S1 { }

  @frozen
#if hasAttribute(foo)
  #if hasAttribute(bar)
  @foo @bar
  #endif
#endif
  public struct S2 { }

#if hasAttribute(foo)
  @foo
#endif
  @inlinable
  func f1() { }

#if hasAttribute(foo)
  @foo
#else
  @available(*, deprecated, message: "nope")
  @frozen
#endif
  public struct S3 { }
}

// Nested in a function
func f2() {
#if hasAttribute(foo)
  @foo(this is gibberish)
#endif
  struct S1 { }

  @available(*, deprecated)
#if hasAttribute(foo)
  @foo
#endif
  struct S2 { }

#if hasAttribute(foo)
  @foo
#endif
  @available(*, deprecated)
  func f1() { }

#if hasAttribute(foo)
  @foo
#else
  @available(*, deprecated, message: "nope")
#endif
  struct S3 { }
}
