import BaseLib

extension Base {
#if VALID
  convenience init(foo: String) {
    fatalError()
  }
#else
  init(foo: String) {
    fatalError()
  }
#endif
}
