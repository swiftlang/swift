import _Differentiation

// Array storage makes both the primal and tangent values nontrivial.
public struct HeapPair: Differentiable, AdditiveArithmetic, Sendable {
  public var values: [Double]

  @differentiable(reverse)
  public init(_ first: Double, _ second: Double) {
    values = [first, second]
  }

  public static var zero: Self { Self(0, 0) }

  @differentiable(reverse)
  public static func + (lhs: Self, rhs: Self) -> Self {
    Self(lhs.values[0] + rhs.values[0], lhs.values[1] + rhs.values[1])
  }

  @differentiable(reverse)
  public static func - (lhs: Self, rhs: Self) -> Self {
    Self(lhs.values[0] - rhs.values[0], lhs.values[1] - rhs.values[1])
  }

  @differentiable(reverse)
  public static func * (lhs: Self, rhs: Self) -> Self {
    Self(lhs.values[0] * rhs.values[0], lhs.values[1] * rhs.values[1])
  }

  // Define += here so its derivative can be synthesized.
  @differentiable(reverse)
  public static func += (lhs: inout Self, rhs: Self) {
    lhs = lhs + rhs
  }
}

#if canImport(Glibc)
import Glibc
#elseif canImport(Darwin)
import Darwin
#elseif canImport(Musl)
import Musl
#elseif canImport(Android)
import Android
#endif

#if canImport(Glibc) || canImport(Darwin) || canImport(Musl) || canImport(Android)
// Preserve an inactive external call in the differentiated task.
@inline(never)
func asyncVJPBlockingSleep(milliseconds: UInt64) {
  var requested = timespec(
    tv_sec: Int(milliseconds / 1_000),
    tv_nsec: Int((milliseconds % 1_000) * 1_000_000)
  )
  var remaining = timespec()
  nanosleep(&requested, &remaining)
}
#endif
