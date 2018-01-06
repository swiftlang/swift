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
  // FIXME: Shared/owned

  // Mix-and-match.
  expectEqual(type(of: f2_variadic_inout), _typeByMangledName("yyytd_ytztc")!)
}

runAllTests()

