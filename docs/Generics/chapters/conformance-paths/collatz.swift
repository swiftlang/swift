protocol Tag {
  associatedtype A: Tag
  associatedtype B: Tag
  associatedtype C: Tag
  associatedtype Del: Tag
  associatedtype Next
}

struct AA<T: Tag>: Tag {
  typealias A = AA<T.A>
  typealias B = AA<T.B>
  typealias C = AA<T.C>
  typealias Del = T
  typealias Next = Del.Del.B.C.Next
}

struct BB<T: Tag>: Tag {
  typealias A = BB<T.A>
  typealias B = BB<T.B>
  typealias C = BB<T.C>
  typealias Del = T
  typealias Next = Del.Del.A.Next
}

struct CC<T: Tag>: Tag {
  typealias A = CC<T.A>
  typealias B = CC<T.B>
  typealias C = CC<T.C>
  typealias Del = T
  typealias Next = Del.Del.A.A.A.Next
}

struct End: Tag {
  typealias A = AA<End>
  typealias B = BB<End>
  typealias C = CC<End>
  typealias Del = Halt
  typealias Next = Halt
}

struct Halt: Tag {
  typealias A = Halt
  typealias B = Halt
  typealias C = Halt
  typealias Del = Halt
  typealias Next = Halt
}

func collatz<T: Tag>(_: T) -> T.Next {
  fatalError()
}

let x = collatz(AA<AA<AA<End>>>())  // what is the type of `x'?
