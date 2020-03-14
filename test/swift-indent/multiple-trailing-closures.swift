// RUN: %swift-indent %s >%t.response
// RUN: diff -u %s %t.response

foo(c: 12, d: 34) {
    a: { print("foo") }
}

foo {
    a: { print("foo") }
}

foo {
    a: { print("foo") }
    b
}

foo (c: 12, d: 34) {
    a: { print("foo") }
    b:
}

// Invalid, but we should still indent correctly.
foo {
    a: {
        print("foo")
    }
    b: bar(a: 1,
           b: 2) {
        print("bar")
    }
}

foo {
    a: {
        print("foo")
    }
    b: {
        print("bar")
    }
}

foo(c: 12, d: 34) {
    a: {
        print("foo")
    }
    b: {
        print("bar")
    }
}

foo(c: 12, d: 34) { a: {
    print("foo")
} b: {
    print("bar")
}}

foo(c: 12, d: 34) { a: {print("foo")}
    b: {print("bar")} }

foo(c: 12, d: 34) { a: {print("foo")}
    /*comment*/b: {
        print("bar")
    }}

foobar(c: 12,
       d: 34) {
    a: {
        print("foo")
    }
    b: {
        print("bar")
    }
}

foo(c: 12, d: 34)
{
    a: {
        print("foo")
    }
    b: {
        print("bar")
    }
}

foobar[c: 12,
       d: 34] {
    a: {
        print("foo")
    }
    b: {
        print("bar")
    }
}

foo(c: 12, d: 34)
{
    a: {
        print("foo")
    }
    // comment
    b: {
        print("bar")
    }

func invalidCode() {
    print("blah")
}