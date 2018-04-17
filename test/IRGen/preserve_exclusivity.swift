// RUN: %target-swift-frontend -parse-stdlib -parse-stdlib -emit-ir -Onone %s | %FileCheck --check-prefix=IR-Onone %s

// We check separately that:
//
// 1. We properly emit our special functions as inline always.
// 2. end-to-end we inline the access markers.

// RUN: %target-swift-frontend -parse-stdlib -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=GenericSpecializer -parse-stdlib -emit-ir -O -disable-llvm-optzns %s | %FileCheck --check-prefix=IR-Osil %s
// RUN: %target-swift-frontend -parse-stdlib -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=GenericSpecializer -parse-stdlib -emit-ir -O %s | %FileCheck --check-prefix=IR-Ollvm %s

@_silgen_name("marker1")
func marker1() -> ()

@_silgen_name("marker2")
func marker2() -> ()

@_silgen_name("marker3")
func marker3() -> ()

@_silgen_name("marker4")
func marker4() -> ()

// IR-Onone: define swiftcc void @"$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*, %swift.type* %T1)
// IR-Onone: call void @swift_beginAccess
// IR-Onone-NEXT: ret void

// IR-Osil: define swiftcc void @"$S20preserve_exclusivity10beginNoOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*, %swift.type* %T1) [[ATTR:#[0-9][0-9]*]] {
// IR-Osil:   call void @swift_beginAccess
// IR-Osil-NEXT: ret void

@inline(never)
@_semantics("optimize.sil.preserve_exclusivity")
public func beginNoOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker1()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// IR-Onone: define swiftcc void @"$S20preserve_exclusivity8endNoOptyyBpF"(i8*)
// IR-Onone: call void @swift_endAccess
// IR-Onone-NEXT: ret void

// IR-Osil: define swiftcc void @"$S20preserve_exclusivity8endNoOptyyBpF"(i8*) [[ATTR]]
// IR-Osil:   call void @swift_endAccess
// IR-Osil-NEXT: ret void
@inline(never)
@_semantics("optimize.sil.preserve_exclusivity")
public func endNoOpt(_ address: Builtin.RawPointer) {
  marker2()
  Builtin.endUnpairedAccess(address)
}

class Klass {}

// Make sure testNoOpt properly inlines in our functions.
//
// IR-Ollvm: define swiftcc void @"$S20preserve_exclusivity9testNoOptyyBpF"(i8*)
// IR-Ollvm: call swiftcc void @marker1
// IR-Ollvm: call void @swift_beginAccess
// IR-Ollvm: call swiftcc void @marker2
// IR-Ollvm: call void @swift_endAccess
// IR-Ollvm-NEXT: ret void
public func testNoOpt(_ k1: Builtin.RawPointer) {
  beginNoOpt(k1, k1, Builtin.RawPointer.self)
  endNoOpt(k1)
}

// IR-Onone: define swiftcc void @"$S20preserve_exclusivity8beginOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*, %swift.type* %T1)
// IR-Onone: call void @swift_beginAccess
// IR-Onone-NEXT: ret void

// IR-Osil: define swiftcc void @"$S20preserve_exclusivity8beginOptyyBp_BpxmtlF"(i8*, i8*, %swift.type*, %swift.type* %T1)
// IR-Osil-NEXT: entry
// IR-Osil-NEXT: call swiftcc void @marker3
// IR-Osil-NEXT: ret void

@inline(never)
public func beginOpt<T1>(_ address: Builtin.RawPointer, _ scratch: Builtin.RawPointer, _ ty1: T1.Type) {
  marker3()
  Builtin.beginUnpairedModifyAccess(address, scratch, ty1);
}

// IR-Onone: define swiftcc void @"$S20preserve_exclusivity6endOptyyBpF"(i8*)
// IR-Onone: call void @swift_endAccess
// IR-Onone-NEXT: ret void

// IR-Osil: define swiftcc void @"$S20preserve_exclusivity6endOptyyBpF"(i8*)
// IR-Osil-NEXT: entry
// IR-Osil-NEXT: call swiftcc void @marker4
// IR-Osil-NEXT: ret void

@inline(never)
public func endOpt(_ address: Builtin.RawPointer) {
  marker4()
  Builtin.endUnpairedAccess(address)
}

public func testOpt(_ k1: Builtin.RawPointer) {
  beginOpt(k1, k1, Builtin.RawPointer.self)
  endOpt(k1)
}

// IR-Osil: attributes [[ATTR]] = { alwaysinline
