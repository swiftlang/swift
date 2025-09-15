// RUN: %target-swift-frontend -disable-availability-checking -typecheck -verify %s

struct Foo<let n: Int> {}

func foo(x: Foo<256>) {}

func bar(x: Foo<0x100>) {
    foo(x: x)
}

func oof(x: Foo<-256>) {}

func rab(x: Foo<-0x100>) {
    oof(x: x)
}
