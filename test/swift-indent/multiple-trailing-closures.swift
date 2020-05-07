// RUN: %swift-indent %s >%t.response
// RUN: diff -u %s %t.response

foo(c: 12, d: 34) {
    print("foo")
}

foo(c: 12, d: 34)
{
    print("foo")
}


someCall(c: 12, d: 34) {
    print("foo")
} e: {
    print("foo")
} f: {
    print("bar")
}

someCall(c: 12, d: 34) {
    print("foo")
}
e: {
    print("foo")
}
f: {
    print("bar")
}

someCall(c: 12, d: 34)
{
    print("foo")
}
e:
{
    print("foo")
}
f:
{
    print("bar")
}

someCall(c: 12, d: 34) { print("foo") }
    e: { print("foo") }
    f: {
        print("bar")
    }

someCall(c: 12,
         d: 34) { print("foo") }
    e: { print("bar") }
    .other { print("foo") }
        a: { print("foo") }
    .next() {
        print("bar")
    }

base
    .someCall(c: 12,
              d: 34) { print("foo") }
        e: { print("bar") }
    .other { print("foo") }
        a: { print("foo") }
    .next() {
        print("bar")
    }

someCall(
    c: 12,
    d: 34
) { print("foo") }
e: { print("bar") }

base
    .someCall(
        x: 10,
        y: 10
    ) { $0 }
    next: { $0 }

base
    .foo(
        x: 10,
        y: 10) { } next: {

    }
    other: {}

base
    .someCall(x: 120,
              y: 3030) { print("foo") } next:
        {
            print("foo")
        }
        other: {}


someCall {
    print("foo")
} e: {
    print("foo")
} f: {
    print("bar")
}

someCall {
    print("foo")
}
e: {
    print("foo")
}
f: {
    print("bar")
}

someCall
{
    print("foo")
}
e:
{
    print("foo")
}
f:
{
    print("bar")
}

someCall { print("foo") }
    e: { print("foo") }
    f: {
        print("bar")
    }

doSomethingWith(someCall { print("foo") }
                    e: { print("foo") }
                    f: {
                        print("bar")
                    })

doSomethingWith(someCall {
    print("foo")
}
e: { print("foo") }
f: {
    print("bar")
})

func containIncomplete1() {
    someCall {
        print("foo")
    }
    e
}

func containIncomplete2() {
    someCall {
        print("foo")
    }
    e:
}

func containIncomplete3() {
    someCall
    {
        print("foo")
    }
    e
}

func containIncomplete4() {
    someCall
    {
        print("foo")
    }
    e:
}

func containIncomplete5() {
    someCall { print("foo") }
    e
}

func containIncomplete6() {
    someCall { print("foo") }
    e:
}

someCall(c: 12, d: 34) {{
    print("foo")
}()} b: {{
    print("bar")
}()}

someCall(c: 12, d: 34) { print("foo") }
    /*comment*/e: { print("bar") }
    /*comment*/f: {
        print("hi")
    }

someCall[c: 12,
         d: 34] { print("foo") }
    b: {
        print("bar")
    }

someSub[c: 12,
        d: 34] {
    print("foo")
} b: {
    print("bar")
}

someSub[c: 12,
        d: 34] {
    print("foo")
} b: {
    print("bar")
}

someCall(c: 12, d: 34) { print("foo") }
    // comment
    b: {
        print("bar")
    }
