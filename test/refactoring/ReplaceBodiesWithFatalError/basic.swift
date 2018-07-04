var foo: Int = 0

var bar: Int {
    return 0
}

var qux: Int = 0 {
    willSet { print("hi") }
    didSet { print("bye") }
}

func generic<T>(_: T) -> Int {
    return 0
}

class Foo {
    var foo: Int

    var bar: Int {
        return 0
    }

    var baz: Int {
        get { return 0 }
        set { foo = newValue }
    }

    var qux: Int {
        willSet { print("hi") }
        didSet { print("bye") }
    }

    init() {
        foo = 0
        qux = 1
    }

    func nonGeneric() -> Int {
        return 0
    }

    func generic<T>(_: T) -> Int {
        return 0
    }

    deinit {
        print("bye")
    }
}

extension Foo {
    func nonGeneric2() -> Int {
        return 0
    }

    func generic2<T>(_: T) -> Int {
        return 0
    }
}

// RUN: %empty-directory(%t.result)

// FIXME: we should be able to run on the whole file.
// RUN: not %refactor -replace-bodies-with-fatalError -source-filename %s -pos=1:1 -end-pos=59:2

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=3:1 -end-pos=5:2 > %t.result/L3-5.swift
// RUN: diff -u %S/Outputs/basic/L3-5.swift.expected %t.result/L3-5.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=7:1 -end-pos=10:2 > %t.result/L7-10.swift
// RUN: diff -u %S/Outputs/basic/L7-10.swift.expected %t.result/L7-10.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=8:1 -end-pos=8:28 > %t.result/L8.swift
// RUN: diff -u %S/Outputs/basic/L8.swift.expected %t.result/L8.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=12:1 -end-pos=14:2 > %t.result/L12-14.swift
// RUN: diff -u %S/Outputs/basic/L12-14.swift.expected %t.result/L12-14.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=16:1 -end-pos=49:2 > %t.result/L16-49.swift
// RUN: diff -u %S/Outputs/basic/L16-49.swift.expected %t.result/L16-49.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=19:1 -end-pos=22:1 > %t.result/L19-22.swift
// RUN: diff -u %S/Outputs/basic/L19-22.swift.expected %t.result/L19-22.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=30:1 -end-pos=44:6 > %t.result/L30-44.swift
// RUN: diff -u %S/Outputs/basic/L30-44.swift.expected %t.result/L30-44.swift

// RUN: %refactor -replace-bodies-with-fatalError -source-filename %s -pos=51:1 -end-pos=59:2 > %t.result/L51-59.swift
// RUN: diff -u %S/Outputs/basic/L51-59.swift.expected %t.result/L51-59.swift
