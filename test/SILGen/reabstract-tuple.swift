// RUN: %target-swift-frontend  -emit-silgen -verify %s

// SR-3090:

class Box<T> {
    public let value: T
    
    public init(_ value: T) {
        self.value = value
    }
}

let box = Box((22, { () in }))
let foo = box.value.0
print(foo)

// Another crash -- re-abstracting function type inside optional in tuple
// in-place

func g<T>() -> (Int, T)? { }

func f<T>(t: T) {
  let _: (Int, ((T) -> (), T))? = g()
}
