// Helper for newtype_conformance.swift

@_exported import MoreSwiftNewtypes

func acceptEquatable<T: Equatable>(_: T) {}

func testEquatable(wrappedRef: WrappedRef, wrappedValue: WrappedValue) {
  acceptEquatable(wrappedRef)
  acceptEquatable(wrappedValue)
}


func intIfNonHashable<T>(_: T) -> Int { return 0 }
func intIfNonHashable<T: Hashable>(_: T) -> Bool { return false }

func testNotHashable(wrappedRef: WrappedRef, wrappedValue: WrappedValue) {
  let refResult = intIfNonHashable(wrappedRef)
  let _: Int = refResult
  let valueResult = intIfNonHashable(wrappedValue)
  let _: Int = valueResult

  let baselineResult = intIfNonHashable(0)
  let _: Bool = baselineResult
}
