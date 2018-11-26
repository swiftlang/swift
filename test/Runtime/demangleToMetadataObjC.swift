// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import StdlibUnittest
import Foundation
import CoreFoundation
import CoreLocation
import Dispatch

let DemangleToMetadataTests = TestSuite("DemangleToMetadataObjC")

@objc class C : NSObject { }
@objc enum E: Int { case a }
@objc protocol P1 { }
protocol P2 { }
@objc protocol P3: P1 { }

DemangleToMetadataTests.test("@objc classes") {
  expectEqual(type(of: C()), _typeByMangledName("4main1CC")!)
}

DemangleToMetadataTests.test("@objc enums") {
  expectEqual(type(of: E.a), _typeByMangledName("4main1EO")!)
}

func f1_composition_objc_protocol(_: P1) { }

DemangleToMetadataTests.test("@objc protocols") {
  expectEqual(type(of: f1_composition_objc_protocol),
              _typeByMangledName("yy4main2P1_pc")!)
}

DemangleToMetadataTests.test("Objective-C classes") {
  expectEqual(type(of: NSObject()), _typeByMangledName("So8NSObjectC")!)
}

func f1_composition_NSCoding(_: NSCoding) { }

DemangleToMetadataTests.test("Objective-C protocols") {
  expectEqual(type(of: f1_composition_NSCoding), _typeByMangledName("yySo8NSCoding_pc")!)
}

DemangleToMetadataTests.test("Classes that don't exist") {
  expectNil(_typeByMangledName("4main4BoomC"))
}

DemangleToMetadataTests.test("CoreFoundation classes") {
  expectEqual(CFArray.self, _typeByMangledName("So10CFArrayRefa")!)
}

DemangleToMetadataTests.test("Imported error types") {
  expectEqual(URLError.self, _typeByMangledName("10Foundation8URLErrorV")!)
  expectEqual(URLError.Code.self,
    _typeByMangledName("10Foundation8URLErrorV4CodeV")!)
}

DemangleToMetadataTests.test("Imported swift_wrapper types") {
  expectEqual(URLFileResourceType.self,
    _typeByMangledName("So21NSURLFileResourceTypea")!)
}

DemangleToMetadataTests.test("Imported enum types") {
  expectEqual(URLSessionTask.State.self,
    _typeByMangledName("So21NSURLSessionTaskStateV")!)
}

class CG4<T: P1, U: P2> { }
extension C : P1 { }
extension C : P2 { }

class D: P2 { }

DemangleToMetadataTests.test("@objc protocol conformances") {
  expectEqual(CG4<C, C>.self,
    _typeByMangledName("4main3CG4CyAA1CCAA1CCG")!)
  expectNil(_typeByMangledName("4main3CG4CyAA1DCAA1DCG"))
}

DemangleToMetadataTests.test("synthesized declarations") {
  expectEqual(CLError.self, _typeByMangledName("SC7CLErrorLeV")!)
  expectNil(_typeByMangledName("SC7CLErrorV"))
  expectEqual(CLError.Code.self, _typeByMangledName("So7CLErrorV")!)

  let error = NSError(domain: NSCocoaErrorDomain, code: 0)
  let reflectionString = String(reflecting: CLError(_nsError: error))
  expectTrue(reflectionString.hasPrefix("__C_Synthesized.related decl 'e' for CLError(_nsError:"))
}

DemangleToMetadataTests.test("members of runtime-only Objective-C classes") {
  expectEqual(DispatchQueue.Attributes.self,
    _typeByMangledName("So17OS_dispatch_queueC8DispatchE10AttributesV")!)
}

DemangleToMetadataTests.test("runtime conformance lookup via foreign superclasses") {
  expectEqual(Set<CFMutableString>.self,
    _typeByMangledName("ShySo18CFMutableStringRefaG")!)
}

class F<T: P1> { }

DemangleToMetadataTests.test("runtime conformance check for @objc protocol inheritance") {
  expectEqual(F<P3>.self, _typeByMangledName("4main1FCyAA2P3PG")!)
}

runAllTests()

