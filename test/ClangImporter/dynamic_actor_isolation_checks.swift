// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-availability-checking -enable-upcoming-feature DynamicActorIsolation -Xllvm -sil-print-types -emit-silgen -module-name preconcurrency_conformances %t/src/checks.swift | %FileCheck %t/src/checks.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-availability-checking -swift-version 6 -disable-dynamic-actor-isolation -Xllvm -sil-print-types -emit-silgen -module-name preconcurrency_conformances %t/src/checks_disabled.swift | %FileCheck %t/src/checks_disabled.swift

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: swift_feature_DynamicActorIsolation

//--- checks.swift
import Foundation

actor MyActor {
}

@globalActor
struct GlobalActor {
  static var shared: MyActor = MyActor()
}

@objc protocol P {
  var data: String? { get set }

  init()
  func test() -> Any?
}

@MainActor
final class K : @preconcurrency P {
  var data: String? {
    get { nil }
    set {}
  }

  init() {}
  @GlobalActor func test() -> Any? { nil }
}

// @objc K.data.getter
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KC4dataSSSgvgTo : $@convention(objc_method) (K) -> @autoreleased Optional<NSString>
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// @objc K.data.setter
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KC4dataSSSgvsTo : $@convention(objc_method) (Optional<NSString>, K) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// @objc K.init()
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KCACycfcTo : $@convention(objc_method) (@owned K) -> @owned K
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// @objc K.test()
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KC4testypSgyFTo : $@convention(objc_method) (K) -> @autoreleased Optional<AnyObject>
// CHECK: [[GLOBAL_ACTOR_METATYPE:%.*]] = metatype $@thin GlobalActor.Type
// CHECK: [[SHARED_ACTOR_GETTER:%.*]] = function_ref @$s27preconcurrency_conformances11GlobalActorV6sharedAA02MyD0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK-NEXT: [[ACTOR_PTR:%.*]] = apply [[SHARED_ACTOR_GETTER]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK-NEXT: [[MY_ACTOR_ADDR:%.*]] = pointer_to_address [[ACTOR_PTR]] : $Builtin.RawPointer to [strict] $*MyActor
// CHECK-NEXT: [[MY_ACTOR_ACCESS:%.*]] = begin_access [read] [dynamic] [[MY_ACTOR_ADDR]] : $*MyActor
// CHECK-NEXT: [[MY_ACTOR:%.*]] = load [copy] [[MY_ACTOR_ACCESS]] : $*MyActor
// CHECK: [[MY_ACTOR_REF:%.*]] = begin_borrow [[MY_ACTOR]] : $MyActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MY_ACTOR_REF]] : $MyActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

@MainActor
class TestObjCMethod {
  @objc func testImplicit() -> Int { 42 }

  @GlobalActor
  @objc func testExplicit() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances14TestObjCMethodC12testImplicitSiyFTo : $@convention(objc_method) (TestObjCMethod) -> Int
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances14TestObjCMethodC12testExplicityyFTo : $@convention(objc_method) (TestObjCMethod) -> ()
// CHECK: [[GLOBAL_ACTOR_METATYPE:%.*]] = metatype $@thin GlobalActor.Type
// CHECK: [[SHARED_ACTOR_GETTER:%.*]] = function_ref @$s27preconcurrency_conformances11GlobalActorV6sharedAA02MyD0Cvau : $@convention(thin) () -> Builtin.RawPointer
// CHECK-NEXT: [[ACTOR_PTR:%.*]] = apply [[SHARED_ACTOR_GETTER]]() : $@convention(thin) () -> Builtin.RawPointer
// CHECK-NEXT: [[MY_ACTOR_ADDR:%.*]] = pointer_to_address [[ACTOR_PTR]] : $Builtin.RawPointer to [strict] $*MyActor
// CHECK-NEXT: [[MY_ACTOR_ACCESS:%.*]] = begin_access [read] [dynamic] [[MY_ACTOR_ADDR]] : $*MyActor
// CHECK-NEXT: [[MY_ACTOR:%.*]] = load [copy] [[MY_ACTOR_ACCESS]] : $*MyActor
// CHECK: [[MY_ACTOR_REF:%.*]] = begin_borrow [[MY_ACTOR]] : $MyActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MY_ACTOR_REF]] : $MyActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

@objcMembers
class Super {
  @MainActor func test() {}
}

class Sub : Super {
  override func test() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances5SuperC4testyyFTo : $@convention(objc_method) (Super) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances3SubC4testyyFTo : $@convention(objc_method) (Sub) -> ()
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

@MainActor
class NSObjectInitOverride: NSObject {
  @MainActor override init() {
    super.init()
  }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances20NSObjectInitOverrideCACycfcTo : $@convention(objc_method) (@owned NSObjectInitOverride) -> @owned NSObjectInitOverride
// CHECK: [[MAIN_ACTOR_METATYPE:%.*]] = metatype $@thick MainActor.Type
// CHECK: [[SHARED_FIELD:%.*]] = function_ref @$sScM6sharedScMvgZ : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[SHARED_ACTOR:%.*]] = apply [[SHARED_FIELD]]([[MAIN_ACTOR_METATYPE]]) : $@convention(method) (@thick MainActor.Type) -> @owned MainActor
// CHECK-NEXT: [[MAIN_ACTOR:%.*]] = begin_borrow [[SHARED_ACTOR]] : $MainActor
// CHECK-NEXT: [[EXEC:%.*]] = extract_executor [[MAIN_ACTOR]] : $MainActor
// CHECK: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
// CHECK-NEXT: {{.*}} = apply [[PRECONDITION]]({{.*}}, [[EXEC]]) : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

//--- checks_disabled.swift
import Foundation

actor MyActor {
}

@globalActor
struct GlobalActor {
  static let shared: MyActor = MyActor()
}

@objc protocol P {
  var data: String? { get set }

  init()
  func test() -> String?
}

@MainActor
final class K : @preconcurrency P {
  var data: String? {
    get { nil }
    set {}
  }

  init() {}
  @GlobalActor func test() -> String? { nil }
}

// @objc K.data.getter
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KC4dataSSSgvgTo : $@convention(objc_method) (K) -> @autoreleased Optional<NSString>
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// @objc K.data.setter
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KC4dataSSSgvsTo : $@convention(objc_method) (Optional<NSString>, K) -> ()
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// @objc K.init()
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KCACycfcTo : $@convention(objc_method) (@owned K) -> @owned K
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// @objc K.test()
// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances1KC4testSSSgyFTo : $@convention(objc_method) (K) -> @autoreleased Optional<NSString>
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

@MainActor
class TestObjCMethod {
  @objc func testImplicit() -> Int { 42 }

  @GlobalActor
  @objc func testExplicit() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances14TestObjCMethodC12testImplicitSiyFTo : $@convention(objc_method) (TestObjCMethod) -> Int
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances14TestObjCMethodC12testExplicityyFTo : $@convention(objc_method) (TestObjCMethod) -> ()
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

@objcMembers
class Super {
  @MainActor func test() {}
}

class Sub : Super {
  override func test() {}
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances5SuperC4testyyFTo : $@convention(objc_method) (Super) -> ()
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances3SubC4testyyFTo : $@convention(objc_method) (Sub) -> ()
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()

@MainActor
class NSObjectInitOverride: NSObject {
  @MainActor override init() {
    super.init()
  }
}

// CHECK-LABEL: sil private [thunk] [ossa] @$s27preconcurrency_conformances20NSObjectInitOverrideCACycfcTo : $@convention(objc_method) (@owned NSObjectInitOverride) -> @owned NSObjectInitOverride
// CHECK-NOT: [[PRECONDITION:%.*]] = function_ref @$ss22_checkExpectedExecutor14_filenameStart01_D6Length01_D7IsASCII5_line9_executoryBp_BwBi1_BwBetF : $@convention(thin) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, Builtin.Word, Builtin.Executor) -> ()
