func getZero() -> Int {
  return 0
}

func getInput(`x: Int) -> Int {
  return x
}

func getSecond(Int, `y: Int) -> Int {
  return y
}

func useNested((x: Int, y: Int), `n: Int) {}

func variadic(`x: Double, y: Int...) {}

func slice(`x: Int[]) {}
func optional(`x: Int?) {}

func overloaded(`x: Int) {}
func overloaded(`x: Bool) {}

// Generic functions.
func makePair<A, B>(`a: A, `b: B) -> (A, B) {
  return (a, b)
}

func different<T : Equatable>(`a: T, `b: T) -> Bool {
  return a != b
}

func different2<T where T : Equatable>(`a: T, `b: T) -> Bool {
  return a != b
}

func selectorFunc1(`a: Int, b x: Int) {}

protocol Wrapped {
  typealias ValueType : Equatable
  
  //var value : ValueType
  func getValue() -> ValueType
}

func differentWrapped<
  T : Wrapped, U : Wrapped
  where
  T.ValueType == U.ValueType
>(`a: T, `b: U) -> Bool {
  return a.getValue() != b.getValue()
}

@noreturn @asmname("exit") func exit ()->()

@noreturn func testNoReturnAttr() -> () { exit() }
@noreturn func testNoReturnAttrPoly<T>(`x: T) -> () { exit() }


@asmname("primitive") func primitive()

protocol EqualOperator {
  func ==(x: Self, y: Self) -> Bool
}
