// RUN: %target-swift-frontend %s -emit-ir > %t.ir
// RUN: %FileCheck --input-file %t.ir %s
// RUN: %FileCheck --check-prefix NEGATIVE --input-file %t.ir %s

// REQUIRES: CPU=i386 || CPU=x86_64 || CPU=arm64

// Non-Swift _silgen_name definitions

@_silgen_name("atan2") func atan2test(_ a: Double, _ b: Double) -> Double
_ = atan2test(0.0, 0.0)
// CHECK: call swiftcc double @atan2(double {{.*}}, double {{.*}})


// Ordinary Swift definitions
// The unused internal and private functions are expected to be kept as they 
// may be used from the debugger in unoptimized builds.

public   func PlainPublic()   { }
internal func PlainInternal() { }
private  func PlainPrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s7asmname11PlainPublic
// CHECK: define{{( dllexport)?}}{{( protected)?}} hidden swiftcc void @"$s7asmname13PlainInternalyyF
// CHECK: define{{( dllexport)?}}{{( protected)?}} internal swiftcc void @"$s7asmname12PlainPrivate


// Swift _silgen_name definitions
// The private function is expected 
// to be eliminated as it may be used from the
// debugger in unoptimized builds,
// and the internal function must survive for C use.
// Only the C-named definition is emitted.

@_silgen_name("silgen_name_public")   public   func SilgenNamePublic()   { }
@_silgen_name("silgen_name_internal") internal func SilgenNameInternal() { }
@_silgen_name("silgen_name_private")  private  func SilgenNamePrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @silgen_name_public
// CHECK: define hidden swiftcc void @silgen_name_internal
// CHECK: define internal swiftcc void @silgen_name_private
// NEGATIVE-NOT: define {{.*}}SilgenName


// Swift cdecl definitions
// The private functions are expected to be kept as it may be used from the debugger,
// and the internal functions must survive for C use.
// Both a C-named definition and a Swift-named definition are emitted.

@_cdecl("cdecl_public")   public   func CDeclPublic()   { }
@_cdecl("cdecl_internal") internal func CDeclInternal() { }
@_cdecl("cdecl_private")  private  func CDeclPrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} void @cdecl_public
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s7asmname11CDeclPublic
// CHECK: define hidden void @cdecl_internal
// CHECK: define hidden swiftcc void @"$s7asmname13CDeclInternal
// CHECK: define internal void @cdecl_private()



// silgen_name on enum constructors
public enum X {
case left(Int64)
case right(Int64)
}

extension X {
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc { i64, i8 } @blah_X_constructor
  @_silgen_name("blah_X_constructor")
  public init(blah: Int64) {
    self = .left(blah)
  }
}



// Swift abi definitions

@abi(func abi_ABIAttrPublic())   public   func api_ABIAttrPublic()   { }
@abi(func abi_ABIAttrInternal()) internal func api_ABIAttrInternal() { }
@abi(func abi_ABIAttrPrivate())  private  func api_ABIAttrPrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s7asmname17abi_ABIAttrPublic
// CHECK: define hidden swiftcc void @"$s7asmname19abi_ABIAttrInternal
// CHECK: define internal swiftcc void @"$s7asmname18abi_ABIAttrPrivate

@abi(var abi_ABIAttrPublic_var: Int64)   public   var api_ABIAttrPublic_var: Int64   { get { 1 } set {} }
@abi(var abi_ABIAttrInternal_var: Int64) internal var api_ABIAttrInternal_var: Int64 { get { 1 } set {} }
@abi(var abi_ABIAttrPrivate_var: Int64)  private  var api_ABIAttrPrivate_var: Int64  { get { 1 } set {} }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc i64 @"$s7asmname21abi_ABIAttrPublic_vars5Int64Vvg"
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s7asmname21abi_ABIAttrPublic_vars5Int64Vvs"
// CHECK: define hidden swiftcc i64 @"$s7asmname23abi_ABIAttrInternal_vars5Int64Vvg"
// CHECK: define hidden swiftcc void @"$s7asmname23abi_ABIAttrInternal_vars5Int64Vvs"
// CHECK: define internal swiftcc i64 @"$s7asmname22abi_ABIAttrPrivate_var33_{{[0-9A-F]+}}LLs5Int64Vvg"
// CHECK: define internal swiftcc void @"$s7asmname22abi_ABIAttrPrivate_var33_{{[0-9A-F]+}}LLs5Int64Vvs"

@abi(func abi_ABIAttrGenericNonSendableToSendable<T>(_ value: T) -> T)
public func api_ABIAttrGenericNonSendableToSendable<T: Sendable>(_ value: T) -> T { return value }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s7asmname031abi_ABIAttrGenericNonSendableToF0yxxlF"
// NEGATIVE-NOT: @"$s7asmname031abi_ABIAttrGenericNonSendableToF0yxxs0F0RzlF"

// Similar to hack applied to `AsyncStream.init(unfolding:onCancel:)`
@abi(func abi_ABIAttrPreconcurrencyToNotPreconcurrency(_ c1: () -> Void, _ c2: @Sendable () -> Void))
@preconcurrency public func api_ABIAttrPreconcurrencyToNotPreconcurrency(_ c1: () -> Void, _ c2: @Sendable () -> Void) {}
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$s7asmname030abi_ABIAttrPreconcurrencyToNotD0yyyyXE_yyYbXEtF"

@abi(var abi_ABIAttrVarEffects: Int)
public var api_ABIAttrVarEffects: Int { get async throws { fatalError() } }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swifttailcc void @"$s7asmname21abi_ABIAttrVarEffectsSivg"
// CHECK: define internal swifttailcc void @"$s7asmname21abi_ABIAttrVarEffectsSivgTY0_"

extension X {
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc { i64, i8 } @"$s7asmname1XO8abi_blahACs5Int64V_tcfC"
  @abi(init(abi_blah: Int64))
  public init(api_blah: Int64) {
    self = .left(api_blah)
  }
}
// NEGATIVE-NOT: define {{.*}}api_
