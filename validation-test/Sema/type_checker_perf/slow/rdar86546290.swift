// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000

func testCollection<C: Collection>(_: (C.Element) -> Void...) -> (C) -> Void {
    fatalError()
}

func test<E: Equatable> (_: E) -> (E) -> Void {
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable
>(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2)
{
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable,
    I3: Equatable
>
(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2,I3)
{
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable,
    I3: Equatable,
    I4: Equatable
>
(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2,I3,I4)
{
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable,
    I3: Equatable,
    I4: Equatable,
    I5: Equatable
>
(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2,I3,I4,I5)
{
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable,
    I3: Equatable,
    I4: Equatable,
    I5: Equatable,
    I6: Equatable
>
(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2,I3,I4,I5,I6)
{
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable,
    I3: Equatable,
    I4: Equatable,
    I5: Equatable,
    I6: Equatable,
    I7: Equatable
>
(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2,I3,I4,I5,I6,I7)
{
    fatalError()
}

func test<
    C: Collection,
    I1: Equatable,
    I2: Equatable,
    I3: Equatable,
    I4: Equatable,
    I5: Equatable,
    I6: Equatable,
    I7: Equatable,
    I8: Equatable
>
(
    _ c: C
)
    -> (C) -> Void
    where C.Element == (I1,I2,I3,I4,I5,I6,I7,I8)
{
    fatalError()
}

func test() {
  let a1: [Int] = [1,2,3]
  let a2: [Int] = [2,1,3]
  let a3: [Int] = [2,3,1]
  let a4: [Int] = [1,3,2]
  let a5: [Int] = [3,1,2]
  let a6: [Int] = [3,2,1]

  testCollection(  // expected-error {{reasonable time}}
      test([1,2,3]),
      test([2,1,3]),
      test([2,3,1]),
      test([1,3,2]),
      test([3,1,2]),
      test([3,2,1])
  ) ([a1,a2,a3,a4,a5,a6])
}
