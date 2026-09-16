struct G<T: Any> {}  // the requirement is useless, but okay
let x = G<Any>()     // fine
