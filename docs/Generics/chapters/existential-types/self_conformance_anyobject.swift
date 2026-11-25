struct G<T: AnyObject> {}
let x = G<AnyObject>()                 // fine
let y = G<any Sequence & AnyObject>()  // not allowed!
