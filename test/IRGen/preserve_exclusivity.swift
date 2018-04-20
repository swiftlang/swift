// RUN: %target-swift-frontend -parse-stdlib -emit-ir -Onone %s | %FileCheck --check-prefix=IR-Onone %s
//
// Check that access markers in @_semantics("optimize.sil.preserve_exclusivity") functions generate runtime calls.

// RUN: %target-swift-frontend -parse-stdlib -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=GenericSpecializer -emit-ir -O %s | %FileCheck --check-prefix=IR-O %s
//
// Check that the -O pipeline preserves the runtime calls for @_semantics("optimize.sil.preserve_exclusivity") functions.

@_silgen_name("marker1")
func marker1() -> ()

@_silgen_name("marker2")
func marker2() -> ()

@_silgen_name("marker3")
func marker3() -> ()

@_silgen_name("marker4")
func marker4() -> ()

@_silgen_name("marker5")
func marker5() -> ()

@_silgen_name("marker6")
func marker6() -> ()

// IR-Onone-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*, %swift.type* %T1)
// IR-Onone: call void @swift_beginAccess
// IR-Onone-NEXT: ret void

// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*{{.*}}, %swift.type*{{.*}} %T1)
// IR-O:   call void @swift_beginAccess
// IR-O-NEXT: ret void

@_semantics("optimize.sil.preserve_exclusivity")
public func beginNoOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker1()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// IR-Onone-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity8endNoOptyyBpF"(i8*)
// IR-Onone: call void @swift_endAccess
// IR-Onone-NEXT: ret void

// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity8endNoOptyyBpF"(i8*)
// IR-O:   call void @swift_endAccess
// IR-O-NEXT: ret void
@_semantics("optimize.sil.preserve_exclusivity")
public func endNoOpt(_ address: Builtin.RawPointer) {
  marker2()
  Builtin.endUnpairedAccess(address)
}

// IR-Onone-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity9readNoOptyyBp_xmtlF"(i8*, %swift.type*, %swift.type* %T1)
// IR-Onone: call void @swift_beginAccess
// IR-Onone: ret void

// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity9readNoOptyyBp_xmtlF"(i8*, %swift.type*{{.*}}, %swift.type*{{.*}} %T1)
// IR-O:   call void @swift_beginAccess
// IR-O: ret void
@_semantics("optimize.sil.preserve_exclusivity")
public func readNoOpt<T1>(_ address: Builtin.RawPointer, _ ty1: T1.Type) {
  marker3()
  Builtin.performInstantaneousReadAccess(address, ty1);
}

// Make sure testNoOpt properly inlines in our functions.
//
// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity9testNoOptyyBpF"(i8*)
// IR-O: call swiftcc void @marker1
// IR-O: call void @swift_beginAccess
// IR-O: call swiftcc void @marker2
// IR-O: call void @swift_endAccess
// IR-O: call swiftcc void @marker3
// IR-O: call void @swift_beginAccess
// IR-O: ret void
public func testNoOpt(_ k1: Builtin.RawPointer) {
  beginNoOpt(k1, k1, Builtin.RawPointer.self)
  endNoOpt(k1)
  readNoOpt(k1, Builtin.RawPointer.self)
}

// IR-Onone-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity8beginOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*, %swift.type* %T1)
// IR-Onone: call void @swift_beginAccess
// IR-Onone-NEXT: ret void

// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity8beginOptyyBp_BpxmtlF"(i8*{{.*}}, i8*{{.*}}, %swift.type*{{.*}}, %swift.type*{{.*}} %T1)
// IR-O-NEXT: entry
// IR-O-NEXT: call swiftcc void @marker4
// IR-O-NEXT: ret void

public func beginOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker4()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// IR-Onone-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity6endOptyyBpF"(i8*)
// IR-Onone: call void @swift_endAccess
// IR-Onone-NEXT: ret void

// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity6endOptyyBpF"(i8*{{.*}})
// IR-O-NEXT: entry
// IR-O-NEXT: call swiftcc void @marker5
// IR-O-NEXT: ret void

public func endOpt(_ address: Builtin.RawPointer) {
  marker5()
  Builtin.endUnpairedAccess(address)
}

// IR-Onone-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity7readOptyyBp_xmtlF"(i8*, %swift.type*, %swift.type* %T1)
// IR-Onone: call void @swift_beginAccess
// IR-Onone: ret void

// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity7readOptyyBp_xmtlF"(i8*{{.*}}, %swift.type*{{.*}}, %swift.type*{{.*}} %T1)
// IR-O-NEXT: entry
// IR-O-NEXT: call swiftcc void @marker6
// IR-O-NEXT: ret void

public func readOpt<T1>(_ address: Builtin.RawPointer, _ ty1: T1.Type) {
  marker6()
  Builtin.performInstantaneousReadAccess(address, ty1);
}

// Make sure testOpt properly inlines in our functions.
//
// IR-O-LABEL: define {{.*}}swiftcc void @"$S20preserve_exclusivity7testOptyyBpF"(i8*{{.*}})
// IR-O: call swiftcc void @marker4
// IR-O: call swiftcc void @marker5
// IR-O: call swiftcc void @marker6
// IR-O: ret void
public func testOpt(_ k1: Builtin.RawPointer) {
  beginOpt(k1, k1, Builtin.RawPointer.self)
  endOpt(k1)
  readOpt(k1, Builtin.RawPointer.self)
}
