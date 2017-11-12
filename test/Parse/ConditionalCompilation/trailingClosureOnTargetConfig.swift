// RUN: %swift -typecheck %s -verify -D FOO -D BAZ -target x86_64-apple-macosx10.9 -parse-stdlib

struct Foo {}

func useFoo(_ foo: Foo) {}

#if arch(x86_64) && os(OSX)
  useFoo(Foo()) // This should not be parsed as a trailing closure
#endif
