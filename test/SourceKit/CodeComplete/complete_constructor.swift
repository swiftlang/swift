
class Foo {
    init(arg1: Int, arg2: Int) {
    }
}

Foo(

// RUN: %sourcekitd-test -req=complete -pos=7:5 %s -- %s > %t.response
// RUN: %diff -u %s.response %t.response
