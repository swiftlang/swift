// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -emit-ir -Onone -target %target-swift-5.7-abi-triple -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %s -module-name main -o %t/onone.ll
// RUN: %target-swift-frontend -emit-ir -O -target %target-swift-5.7-abi-triple -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %s -module-name main -o %t/o.ll
// RUN: %FileCheck %s --check-prefix=ONONE < %t/onone.ll
// RUN: %FileCheck %s --check-prefix=SETTER < %t/onone.ll
// RUN: %FileCheck %s --check-prefix=OPT < %t/o.ll
// RUN: %FileCheck %s --check-prefix=TYPES < %t/onone.ll
// RUN: not grep -E '"symbolic [^"]*Yo' %t/onone.ll %t/o.ll

// Without the '@available' annotation the 'oneway' distributed func is
// rejected for this deployment target, see
// distributed_actor_oneway_availability.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -I %t %s -module-name main -D UNANNOTATED

// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait
// REQUIRES: OS=macosx

// A 'oneway' distributed func and a 'nowait' call of it, built for a
// deployment target which predates 'RemoteCallTarget.isOnewayRemoteCall'
// (Swift 6.5). The 'oneway' distributed func itself requires Swift 6.5, but
// synthesized code is not availability checked, so the thunk must not set the
// flag unconditionally: it calls the emitted-into-client
// '_markOnewayRemoteCall()', which only sets it when running on a Swift 6.5
// runtime, and references the setter weakly
//
// Also, the type names emitted for runtime use (metadata and reflection) must
// not contain the 'oneway' ('Yo') function type flavor, which older runtimes
// cannot demangle and which runtime metadata does not represent anyway

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Greeter {
  @available(SwiftStdlib 6.5, *)
  distributed func hello() oneway {}
}

#if UNANNOTATED
distributed actor UnannotatedGreeter { // expected-note{{add '@available' attribute to enclosing distributed actor}}
  distributed func hello() oneway {}
  // expected-error@-1{{'oneway' distributed instance method 'hello()' is only available in macOS 99.99.0 or newer}}
  // expected-note@-2{{add '@available' attribute to enclosing distributed instance method}}
}
#endif

@available(SwiftStdlib 6.5, *)
func send(_ g: Greeter) async throws {
  nowait g.hello()
}

actor Worker {
  func work() oneway {}
}

func references(_ w: isolated Worker) -> Any {
  let f = w.work
  return [f]
}

// The thunk calls the helper, which checks the OS version before it calls the
// weakly referenced setter
// ONONE: call swiftcc void @"$s11Distributed16RemoteCallTargetV011_markOnewaybC0yyF"(
// ONONE: define linkonce_odr hidden swiftcc void @"$s11Distributed16RemoteCallTargetV011_markOnewaybC0yyF"(
// ONONE-NEXT: entry:
// ONONE-NEXT: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 9999, i64 0, i64 0)
// ONONE: call swiftcc void @"$s11Distributed16RemoteCallTargetV08isOnewaybC0Sbvs"(i1 true
// ONONE: ret void
// ONONE: declare extern_weak swiftcc void @"$s11Distributed16RemoteCallTargetV08isOnewaybC0Sbvs"(

// That is the only call of the setter
// SETTER: call swiftcc void @"$s11Distributed16RemoteCallTargetV08isOnewaybC0Sbvs"(
// SETTER-NOT: call swiftcc void @"$s11Distributed16RemoteCallTargetV08isOnewaybC0Sbvs"(

// With optimizations the helper is inlined into the thunk
// OPT-NOT: _markOnewaybC0yyF
// OPT: call swiftcc i1 @"$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF"(i64 9999, i64 0, i64 0)
// OPT-NEXT: br i1
// OPT-NOT: _markOnewaybC0yyF
// OPT: call swiftcc void @"$s11Distributed16RemoteCallTargetV08isOnewaybC0Sbvs"(i1 true
// OPT-NOT: _markOnewaybC0yyF
// OPT: declare extern_weak swiftcc void @"$s11Distributed16RemoteCallTargetV08isOnewaybC0Sbvs"(
// OPT-NOT: _markOnewaybC0yyF

// The metadata of '() oneway -> ()' and of '[() oneway -> ()]' is requested
// by the mangled names of '() -> ()' and of '[() -> ()]'
// TYPES-DAG: @"symbolic yyc" =
// TYPES-DAG: @"symbolic SayyycG" =
