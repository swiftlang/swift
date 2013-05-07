// WORKAROUND: This is a hack to allow us to emulate iVars of generic
// parameter type.  See <rdar://problem/13560747> IRGen explodes on
// ivar of generic parameter type
struct GenericIVar<T> {
  constructor() {
    __value = new T[1]
  }
  constructor(x: T) {
    __value = new T[1]
    __value[0] = x
  }
  var value : T {
  get:
    return __value[0]
  set(x):
    __value[0] = x
  }
  var __value: T[]
}
