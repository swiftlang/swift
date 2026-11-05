// RUN: %target-swift-frontend %s -emit-sil -target %target-swift-5.7-abi-triple -O -sil-verify-all -module-name test -parse-as-library | %FileCheck %s

// Check that specializing a guaranteed class-to-class cast doesn't result in
// invalid OSSA if a generic argument is substituted with a parameterized
// existential.

public protocol P<T> {
  associatedtype T
}

open class Kind<F, A> {
  public init() {}
}

public final class EitherPartial<L> {}

public final class Either<L, R>: Kind<EitherPartial<L>, R> {
  public let r: R

  public init(_ r: R) { self.r = r }

  @inline(never)
  public static func fix(_ fa: Kind<EitherPartial<L>, R>) -> Either<L, R> {
    return fa as! Either<L, R>
  }
}

// CHECK-LABEL: sil shared [noinline] @$s4test6EitherC3fixyACyxq_GAA4KindCyAA0B7PartialCyxGq_GFZSi_AA1P_pSi1TAaLPRts_XPTt0g5{{.*}} : $@convention(thin) (@guaranteed Kind<EitherPartial<Int>, any P<Int>>) -> Either<Int, any P<Int>>
// CHECK:         unconditional_checked_cast %0 to Either<Int, any P<Int>>
// CHECK:       } // end sil function '$s4test6EitherC3fixyACyxq_GAA4KindCyAA0B7PartialCyxGq_GFZSi_AA1P_pSi1TAaLPRts_XPTt0g5{{.*}}'
public func test(_ k: Kind<EitherPartial<Int>, any P<Int>>) -> Either<Int, any P<Int>> {
  return Either.fix(k)
}

public protocol Q: AnyObject {}

open class C<T> {
  public init() {}
}

@inline(never)
public func castToComposition<T>(_ c: C<T>) -> any C<T> & Q {
  return c as! any C<T> & Q
}

// CHECK-LABEL: sil shared [noinline] @$s4test17castToCompositionyAA1Q_AA1CCyxGXcAFlFAA1P_pSi1TAaGPRts_XP_Tg5{{.*}} : $@convention(thin) (@guaranteed C<any P<Int>>) -> any C<any P<Int>> & Q
// CHECK:         unconditional_checked_cast {{.*}}%0 to any C<any P<Int>> & Q
// CHECK:       } // end sil function '$s4test17castToCompositionyAA1Q_AA1CCyxGXcAFlFAA1P_pSi1TAaGPRts_XP_Tg5{{.*}}'
public func testComposition(_ c: C<any P<Int>>) -> any C<any P<Int>> & Q {
  return castToComposition(c)
}
