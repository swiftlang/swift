// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

let DemangleToMetadataTests = TestSuite("DemangleToMetadata")


DemangleToMetadataTests.test("malformed mangled names") {
  expectNil(_typeByMangledName("blah"))
}

DemangleToMetadataTests.test("tuple types") {
  expectEqual(type(of: ()), _typeByMangledName("yt")!)
  expectEqual(type(of: ((), ())), _typeByMangledName("yt_ytt")!)
  expectEqual(type(of: ((), b: ())), _typeByMangledName("yt_yt1bt")!)
  expectEqual(type(of: (a: (), ())), _typeByMangledName("yt1a_ytt")!)
  expectEqual(type(of: (a: (), b: ())), _typeByMangledName("yt1a_yt1bt")!)
}

func f0() { }
var f0_thin: @convention(thin) () -> Void = f0
var f0_c: @convention(c) () -> Void = f0

#if _runtime(_ObjC)
var f0_block: @convention(block) () -> Void = f0
#endif

func f0_throws() throws { }

func f1(x: ()) { }
func f2(x: (), y: ()) { }

func f1_variadic(x: ()...) { }
func f1_inout(x: inout ()) { }
func f1_shared(x: __shared AnyObject) { }
func f1_owned(x: __owned AnyObject) { }

func f2_variadic_inout(x: ()..., y: inout ()) { }

DemangleToMetadataTests.test("function types") {
  // Conventions
  expectEqual(type(of: f0), _typeByMangledName("yyc")!)
  expectEqual(type(of: f0_thin), _typeByMangledName("yyXf")!)
  expectEqual(type(of: f0_c), _typeByMangledName("yyXC")!)
#if _runtime(_ObjC)
  expectEqual(type(of: f0_block), _typeByMangledName("yyXB")!)
#endif

  // Throwing functions
  expectEqual(type(of: f0_throws), _typeByMangledName("yyKc")!)

  // More parameters.
  expectEqual(type(of: f1), _typeByMangledName("yyyt_tc")!)
  expectEqual(type(of: f2), _typeByMangledName("yyyt_yttc")!)

  // Variadic parameters.
  expectEqual(type(of: f1_variadic), _typeByMangledName("yyytd_tc")!)

  // Inout parameters.
  expectEqual(type(of: f1_inout), _typeByMangledName("yyytzc")!)

  // Ownership parameters.
  expectEqual(type(of: f1_shared), _typeByMangledName("yyyXlhc")!)
  expectEqual(type(of: f1_owned), _typeByMangledName("yyyXlc")!)

  // Mix-and-match.
  expectEqual(type(of: f2_variadic_inout), _typeByMangledName("yyytd_ytztc")!)
}

DemangleToMetadataTests.test("metatype types") {
  expectEqual(type(of: type(of: ())), _typeByMangledName("ytm")!)
  expectEqual(type(of: type(of: f0)), _typeByMangledName("yycm")!)
}

func f2_any_anyobject(_: Any, _: AnyObject) { }

DemangleToMetadataTests.test("existential types") {
  // Any, AnyObject
  expectEqual(type(of: f2_any_anyobject), _typeByMangledName("yyyp_yXltc")!)

  // FIXME: References to protocols.
  // FIXME: References to superclass.
}

DemangleToMetadataTests.test("existential metatype types") {
  // Any
  expectEqual(type(of: Any.self), _typeByMangledName("ypm")!)

  // AnyObject
  expectEqual(type(of: AnyObject.self), _typeByMangledName("yXlm")!)
}

struct S {
  struct Nested { }
}

enum E { case e }

class C { }

DemangleToMetadataTests.test("nominal types") {
  // Simple Struct
  expectEqual(type(of: S()), _typeByMangledName("4main1SV")!)

  // Simple Enum
  expectEqual(type(of: E.e), _typeByMangledName("4main1EO")!)

  // Simple Class
  expectEqual(type(of: C()), _typeByMangledName("4main1CC")!)

  // Swift standard library types
  expectEqual(type(of: Int()), _typeByMangledName("Si")!)
  expectEqual(type(of: Int16()), _typeByMangledName("s5Int16V")!)

  // Nested struct
  expectEqual(type(of: S.Nested()), _typeByMangledName("4main1SV6NestedV")!)

  // Class referenced by "ModuleName.ClassName" syntax.
  expectEqual(type(of: C()), _typeByMangledName("main.C")!)
}

runAllTests()

