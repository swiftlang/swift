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
