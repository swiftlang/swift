// RUN: %swift-syntax-test -input-source-filename %s -serialize-raw-tree > %t
// RUN: diff %t %S/Inputs/serialize_main_actor.json -u

struct Foo {
    init(_ foo: @MainActor () -> Void) {
        
    }
}

struct Bar {
    var body: Foo {
        Foo { @MainActor in
            print("Hi")
        }
    }
}