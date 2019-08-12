// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/structural_types -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test %t/structural_types -type-from-mangled=%t/input | %FileCheck %s

func blackHole(_: Any...) {}

do {
  let metatype = Int.self
  blackHole(metatype)
}

do {
  let fn: () -> () = { }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (inout String) -> () = { _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (__owned String) -> () = { _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (Int, Float) -> () = { _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (inout Int, Float) -> () = { _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (inout Int, inout Float) -> () = { _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (Int, inout Float) -> () = { _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (Int, inout String, Float) -> () = { _, _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (inout Int, String, inout Float, Double) -> () = { _, _, _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (String, Int, Double, Float) -> () = { _, _, _, _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: ((Int, Float)) -> () = { _ in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let fn: (Int...) -> () = { (_: Int...) in }
  blackHole(fn)
  let metatype = type(of: fn)
  blackHole(metatype)
}

do {
  let tuple: (Int, Float, Int) = (0, 0, 0)
  blackHole(tuple)
  let metatype = type(of: tuple)
  blackHole(metatype)
}

do {
  let tuple: (Int.Type, x: Float, Int) = (Int.self, 0, 0)
  blackHole(tuple)
  let metatype = type(of: tuple)
  blackHole(metatype)
}

do {
  let tuple: (x: Int, Float, y: Int.Type) = (0, 0, Int.self)
  blackHole(tuple)
  let metatype = type(of: tuple)
  blackHole(metatype)
}

do {
  let escaping: (@escaping () -> ()) -> () = { _ in }
  blackHole(escaping)
}

do {
  let c: [@convention(c) () -> ()] = []
  blackHole(c)
}

do {
  let b: [(@escaping @convention(block) () -> (), @convention(block) () -> ()) -> ()] = []
  blackHole(b)
}

// SWIFT_ENABLE_TENSORFLOW
do {
  let f: @differentiable (Float) -> Float = { $0 }
  // FIXME(TF-123): `@differentiable` function type + opaque abstraction
  // pattern bug.
  // blackHole(f)
  _ = f
}

do {
  let f: (@escaping @differentiable (Float) -> Float) -> () = { _ in }
  // FIXME(TF-123): `@differentiable` function type + opaque abstraction
  // pattern bug.
  // blackHole(f)
  _ = f
}

// TODO: Uncomment when `@differentiable(linear)` function types are enabled.
/*
do {
  let f: @differentiable(linear) (Float) -> Float = { $0 }
  // FIXME(TF-123): `@differentiable` function type + opaque abstraction
  // pattern bug.
  // blackHole(f)
  _ = f
}

do {
  let f: (@escaping @differentiable(linear) (Float) -> Float) -> () = { _ in }
  // FIXME(TF-123): `@differentiable` function type + opaque abstraction
  // pattern bug.
  // blackHole(f)
  _ = f
}
*/
// SWIFT_ENABLE_TENSORFLOW END

// DEMANGLE: $syycD
// DEMANGLE: $sySSzcD
// DEMANGLE: $sySSncD
// DEMANGLE: $sySi_SftcD
// DEMANGLE: $sySiz_SftcD
// DEMANGLE: $sySiz_SfztcD
// DEMANGLE: $sySi_SfztcD
// DEMANGLE: $sySi_SSzSftcD
// DEMANGLE: $sySiz_SSSfzSdtcD
// DEMANGLE: $sySS_SiSdSftcD
// DEMANGLE: $sySi_Sft_tcD
// DEMANGLE: $sySid_tcD
// DEMANGLE: $sSi_SfSitD
// DEMANGLE: $sSim_Sf1xSitD
// DEMANGLE: $sSi1x_SfSim1ytD
// DEMANGLE: $syyyccD
// DEMANGLE: $sSayyyXCGD
// DEMANGLE: $sSayyyyXL_yyXBtcGD

// SWIFT_ENABLE_TENSORFLOW
// DEMANGLE: $sS2fXFD
// DEMANGLE: $sS2fXGD
// DEMANGLE: $sS2fXHD
// DEMANGLE: $sS2fXID
// SWIFT_ENABLE_TENSORFLOW END

// CHECK: () -> ()
// CHECK: (inout String) -> ()
// CHECK: (__owned String) -> ()
// CHECK: (Int, Float) -> ()
// CHECK: (inout Int, Float) -> ()
// CHECK: (inout Int, inout Float) -> ()
// CHECK: (Int, inout Float) -> ()
// CHECK: (Int, inout String, Float) -> ()
// CHECK: (inout Int, String, inout Float, Double) -> ()
// CHECK: (String, Int, Double, Float) -> ()
// CHECK: ((Int, Float)) -> ()
// CHECK: (Int...) -> ()
// CHECK: (Int, Float, Int)
// CHECK: (Int.Type, x: Float, Int)
// CHECK: (x: Int, Float, y: Int.Type)
// CHECK: (@escaping () -> ()) -> ()
// CHECK: Array<@convention(c) () -> ()>
// CHECK: Array<(@escaping @convention(block) () -> (), @convention(block) () -> ()) -> ()>

// SWIFT_ENABLE_TENSORFLOW
// CHECK: @differentiable (Float) -> Float
// CHECK: @differentiable (Float) -> Float
// CHECK: @differentiable(linear) (Float) -> Float
// CHECK: @differentiable(linear) (Float) -> Float
// SWIFT_ENABLE_TENSORFLOW END

// DEMANGLE: $sSimD
// DEMANGLE: $syycmD
// DEMANGLE: $sySSzcmD
// DEMANGLE: $sySSncmD
// DEMANGLE: $sySi_SftcmD
// DEMANGLE: $sySiz_SftcmD
// DEMANGLE: $sySiz_SfztcmD
// DEMANGLE: $sySi_SfztcmD
// DEMANGLE: $sySi_SSzSftcmD
// DEMANGLE: $sySiz_SSSfzSdtcmD
// DEMANGLE: $sySS_SiSdSftcmD
// DEMANGLE: $sySi_Sft_tcmD
// DEMANGLE: $sySid_tcmD
// DEMANGLE: $sSi_SfSitmD
// DEMANGLE: $sSim_Sf1xSitmD
// DEMANGLE: $sSi1x_SfSim1ytmD
// DEMANGLE: $syyyccmD
// DEMANGLE: $sSayyyXCGmD
// DEMANGLE: $sSayyyyXL_yyXBtcGmD

// SWIFT_ENABLE_TENSORFLOW
// DEMANGLE: $sS2fXFmD
// DEMANGLE: $sS2fXGmD
// DEMANGLE: $sS2fXHmD
// DEMANGLE: $sS2fXImD
// SWIFT_ENABLE_TENSORFLOW END

// CHECK: Int.Type
// CHECK: ((inout String) -> ()).Type
// CHECK: ((__owned String) -> ()).Type
// CHECK: ((Int, Float) -> ()).Type
// CHECK: ((inout Int, Float) -> ()).Type
// CHECK: ((inout Int, inout Float) -> ()).Type
// CHECK: ((Int, inout Float) -> ()).Type
// CHECK: ((Int, inout String, Float) -> ()).Type
// CHECK: ((inout Int, String, inout Float, Double) -> ()).Type
// CHECK: ((String, Int, Double, Float) -> ()).Type
// CHECK: (((Int, Float)) -> ()).Type
// CHECK: ((Int...) -> ()).Type
// CHECK: (Int, Float, Int).Type
// CHECK: (Int.Type, x: Float, Int).Type
// CHECK: (x: Int, Float, y: Int.Type).Type
// CHECK: ((@escaping () -> ()) -> ()).Type
// CHECK: Array<@convention(c) () -> ()>.Type
// CHECK: Array<(@escaping @convention(block) () -> (), @convention(block) () -> ()) -> ()>.Type

// SWIFT_ENABLE_TENSORFLOW
// CHECK: (@differentiable (Float) -> Float).Type
// CHECK: (@differentiable (Float) -> Float).Type
// CHECK: (@differentiable(linear) (Float) -> Float).Type
// CHECK: (@differentiable(linear) (Float) -> Float).Type
// SWIFT_ENABLE_TENSORFLOW END
