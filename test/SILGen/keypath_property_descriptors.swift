// RUN: %target-swift-emit-silgen %s | %FileCheck --check-prefix=CHECK --check-prefix=NONRESILIENT %s
// RUN: %target-swift-emit-silgen -enable-library-evolution %s | %FileCheck --check-prefix=CHECK --check-prefix=RESILIENT %s
// RUN: %target-swift-emit-silgen -enable-private-imports %s | %FileCheck --check-prefix=PRIVATEIMPORTS %s

// TODO: globals should get descriptors
public var a: Int = 0

@inlinable
public var b: Int { return 0 }

@usableFromInline
internal var c: Int = 0

// no descriptor
// CHECK-NOT: sil_property instance #d
// PRIVATEIMPORTS-NOT: sil_property instance #d
internal var d: Int = 0
// CHECK-NOT: sil_property instance #e
// PRIVATEIMPORTS-NOT: sil_property instance #e
private var e: Int = 0

public struct A {
  // NONRESILIENT-LABEL: sil_property instance #A.a ()
  // RESILIENT-LABEL: sil_property instance #A.a (stored_property
	// PRIVATEIMPORTS-LABEL: sil_property instance #A.a ()
  public var a: Int = 0

  // CHECK-LABEL: sil_property instance #A.b ()
	// PRIVATEIMPORTS-LABEL: sil_property instance #A.b ()
  @inlinable
  public var b: Int { return 0 }

  // NONRESILIENT-LABEL: sil_property instance #A.c ()
  // RESILIENT-LABEL: sil_property instance #A.c (stored_property
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.c ()
  @usableFromInline
  internal var c: Int = 0

  // no descriptor
  // CHECK-NOT: sil_property instance #A.d
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.d ()
  internal var d: Int = 0
  // CHECK-NOT: sil_property instance #A.e
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.e ()
  fileprivate var e: Int = 0
  // CHECK-NOT: sil_property instance #A.f
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.f ()
  private var f: Int = 0

  // static vars
  
  // NONRESILIENT-LABEL: sil_property type #A.a ()
  // RESILIENT-LABEL: sil_property type #A.a (settable_property
  public static var a: Int = 0
  
  // CHECK-LABEL: sil_property type #A.b ()
  @inlinable
  public static var b: Int { return 0 }
  
  // NONRESILIENT-LABEL: sil_property type #A.c ()
  // RESILIENT-LABEL: sil_property type #A.c (settable_property
  @usableFromInline
  internal static var c: Int = 0

  // no descriptor
  // CHECK-NOT: sil_property instance #A.d
  internal static var d: Int = 0
  // CHECK-NOT: sil_property instance #A.e
  fileprivate static var e: Int = 0
  // CHECK-NOT: sil_property instance #A.f
  private static var f: Int = 0

  // CHECK-LABEL: sil_property instance #A.subscript{{.*}} (){{$}}
  public subscript(a x: Int) -> Int { return x }
  // CHECK-LABEL: sil_property instance #A.subscript{{.*}} (){{$}}
  @inlinable
  public subscript(b x: Int) -> Int { return x }
  // CHECK-LABEL: sil_property instance #A.subscript{{.*}} (){{$}}
  @usableFromInline
  internal subscript(c x: Int) -> Int { return x }
  
  // no descriptor
  // CHECK-NOT: sil_property instance #A.subscript
  internal subscript(d x: Int) -> Int { return x }
  fileprivate subscript(e x: Int) -> Int { return x }
  private subscript(f x: Int) -> Int { return x }

  // CHECK-LABEL: sil_property instance #A.subscript{{.*}} (){{$}}
  public subscript<T>(a x: T) -> T { return x }
  // CHECK-LABEL: sil_property instance #A.subscript{{.*}} (){{$}}
  @inlinable
  public subscript<T>(b x: T) -> T { return x }
  // CHECK-LABEL: sil_property instance #A.subscript{{.*}} (){{$}}
  @usableFromInline
  internal subscript<T>(c x: T) -> T { return x }
  
  // no descriptor
  // CHECK-NOT: sil_property instance #A.subscript
  internal subscript<T>(d x: T) -> T { return x }
  fileprivate subscript<T>(e x: T) -> T { return x }
  private subscript<T>(f x: T) -> T { return x }

  // no descriptor
  // CHECK-NOT: sil_property instance #A.count
  public var count: Int {
    mutating get {
      _count += 1
      return _count
    }
    set {
      _count = newValue
    }
  }

  // CHECK-NOT: sil_property instance #A._count
  private var _count: Int = 0

  // NONRESILIENT-LABEL: sil_property instance #A.getSet ()
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.getSet ()
  // RESILIENT-LABEL: sil_property instance #A.getSet (settable_property
  public var getSet: Int {
    get { return 0 }
    set { }
  }

  // CHECK-LABEL: sil_property instance #A.hiddenSetter (settable_property
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.hiddenSetter (settable_property
  public internal(set) var hiddenSetter: Int {
    get { return 0 }
    set { }
  }

  // PRIVATEIMPORTS-LABEL: sil_property instance #A.privateSetter (settable_property
  public private(set) var privateSetter: Int {
    get { return 0 }
    set { }
  }
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.fileprivateSetter (settable_property
  public fileprivate(set) var fileprivateSetter: Int {
    get { return 0 }
    set { }
  }

  // NONRESILIENT-LABEL: sil_property instance #A.usableFromInlineSetter ()
  // PRIVATEIMPORTS-LABEL: sil_property instance #A.usableFromInlineSetter ()
  // RESILIENT-LABEL: sil_property instance #A.usableFromInlineSetter (settable_property
  public internal(set) var usableFromInlineSetter: Int {
    get { return 0 }
    @usableFromInline set { }
  }
}

@_fixed_layout
public struct FixedLayout {
  // NONRESILIENT-LABEL: sil_property instance #FixedLayout.a ()
  // RESILIENT-LABEL: sil_property instance #FixedLayout.a (stored_property
  public var a: Int
  // NONRESILIENT-LABEL: sil_property instance #FixedLayout.b ()
  // RESILIENT-LABEL: sil_property instance #FixedLayout.b (stored_property
  public var b: Int
  // NONRESILIENT-LABEL: sil_property instance #FixedLayout.c ()
  // RESILIENT-LABEL: sil_property instance #FixedLayout.c (stored_property
  public var c: Int
}

public class Foo {}
extension Array where Element == Foo {
  public class Bar {
    // NONRESILIENT-LABEL: sil_property instance #Array.Bar.dontCrash<τ_0_0 where τ_0_0 == Foo> (settable_property $Int
    public private(set) var dontCrash : Int {
      get {
        return 10
      }
      set {
      }
    }
  }
}
