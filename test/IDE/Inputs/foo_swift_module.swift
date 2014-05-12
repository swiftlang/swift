operator infix %%% {
  associativity left
  precedence 200
}

func %%% (lhs: Int, rhs: Int) -> Int {
  return lhs + rhs
}

func visibleImport() {}
func hiddenImport() {}

func overlayedFoo() {}

/// FooSwiftStruct Aaa.
/**
 * Bbb.
 * Ccc.
 */
struct FooSwiftStruct {
  // Indentation is incorrect on purpose, don't fix this.

    /// fooInstanceFunc Aaa.
    /**
     * Bbb
     */
      /**
       * Ccc.
       */
    func fooInstanceFunc() {}
}
struct BarGenericSwiftStruct1<T> {
  init(t: T) {}
  func bar1InstanceFunc() {}
}
protocol BarProtocol {
  func instanceFunc()
}
struct BarGenericSwiftStruct2<T: BarProtocol, U> {
  init(t: T, u: U) {}
  func bar2InstanceFunc() {}
}

