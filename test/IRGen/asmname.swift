// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=i386 || CPU=x86_64


// Non-Swift _silgen_name definitions

@_silgen_name("atan2") func atan2test(_ a: Double, _ b: Double) -> Double
_ = atan2test(0.0, 0.0)
// CHECK: call swiftcc double @atan2(double {{.*}}, double {{.*}})


// Ordinary Swift definitions
// The unused internal and private functions are expected to be eliminated.

public   func PlainPublic()   { }
internal func PlainInternal() { }
private  func PlainPrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S7asmname11PlainPublic
// CHECK-NOT: PlainInternal
// CHECK-NOT: PlainPrivate


// Swift _silgen_name definitions
// The private function is expected to be eliminated
// but the internal function must survive for C use.
// Only the C-named definition is emitted.

@_silgen_name("silgen_name_public")   public   func SilgenNamePublic()   { }
@_silgen_name("silgen_name_internal") internal func SilgenNameInternal() { }
@_silgen_name("silgen_name_private")  private  func SilgenNamePrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @silgen_name_public
// CHECK: define hidden swiftcc void @silgen_name_internal
// CHECK-NOT: silgen_name_private
// CHECK-NOT: SilgenName


// Swift cdecl definitions
// The private functions are expected to be eliminated
// but the internal functions must survive for C use.
// Both a C-named definition and a Swift-named definition are emitted.

@_cdecl("cdecl_public")   public   func CDeclPublic()   { }
@_cdecl("cdecl_internal") internal func CDeclInternal() { }
@_cdecl("cdecl_private")  private  func CDeclPrivate()  { }
// CHECK: define{{( dllexport)?}}{{( protected)?}} void @cdecl_public
// CHECK: define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S7asmname11CDeclPublic
// CHECK: define hidden void @cdecl_internal
// CHECK: define hidden swiftcc void @"$S7asmname13CDeclInternal
// CHECK-NOT: cdecl_private
