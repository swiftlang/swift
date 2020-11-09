class Foo {
  var x: Int
  var y: Int
  func fooMethod() {}
}
struct Bar {
  var a: Int
  var b: Int
  func barMethod() {}
}
func foo(arg: Foo) {
  _ = arg.
}
func bar(arg: Bar) {
  _ = arg.
}

// NOTE: Test for simultaneous completion requests don't cause compiler crashes.

// ReuseASTContext disabled.
// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_max_astcontext_reuse_count=0 \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=17:1 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=17:1 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s

// ReuseASTContext enabled.
// RUN: %sourcekitd-test \
// RUN:   -req=global-config -req-opts=completion_max_astcontext_reuse_count=5 \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=17:1 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s == \
// RUN:   -req=complete -pos=17:1 %s -async -- %s == \
// RUN:   -req=complete -pos=12:11 %s -async -- %s == \
// RUN:   -req=complete -pos=15:11 %s -async -- %s
