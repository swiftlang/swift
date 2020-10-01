// RUN: %target-swift-frontend %s -emit-ir

// SR-9954 / rdar://problem/48223824
// Rejects well-formed that triggered a fallback diagnostic due to a bad
// substitution.
struct GenericThing <Param1, Param2> {
    init (closure: (String)->()) {
        
    }
}

struct ThingHolder <Param1> {
    func acceptThing <Param2> (thingGenerator: ()->GenericThing<Param1, Param2>) {

    }
}

struct A { }

func demo <Param1> (thingHolder: ThingHolder<Param1>) {
    typealias Thing <Param2> = GenericThing<Param1, Param2>
    thingHolder.acceptThing {
        Thing<A> { string in

        }
    }
}

