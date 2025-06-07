// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-objc-attr-requires-foundation-module -enable-objc-interop %t_no_errors.swift
//
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_AD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PRIVATE_AD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_FILEPRIVATE_AD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PRIVATE_AD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_AD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_INTERNAL_AD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_AD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PUBLIC_AD
//
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_BD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PRIVATE_BD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_FILEPRIVATE_BD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PRIVATE_BD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_BD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_INTERNAL_BD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_BD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PUBLIC_BD
//
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_CD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PRIVATE_CD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_FILEPRIVATE_CD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PRIVATE_CD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_CD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_INTERNAL_CD
// RUN: %target-swift-ide-test -enable-objc-interop -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_CD -code-completion-keywords=false | %FileCheck %s -check-prefix=TEST_PUBLIC_CD

@objc
private class TagPA {}

@objc
class TagPB {}

@objc
public class TagPC {}

@objc
private class BaseAPrivate {
  fileprivate init(fromBase: TagPA) {}

  fileprivate func baseAFunc(x: TagPA) {}

  fileprivate subscript(a: TagPA) -> Int { return 1 }

  fileprivate var baseAVarRW: TagPA
  fileprivate let baseAVarRO: TagPA

  fileprivate func colliding() {}
  fileprivate func collidingGeneric<T>(x: T) {}
}

@objc
class BaseBInternal {
  init(fromBaseB: TagPB) {}

  func baseBFunc(x: TagPB) {}

  subscript(a: TagPB) -> Int { return 1 }

  var baseBVarRW: TagPB
  let baseBVarRO: TagPB

  func colliding() {}
  func collidingGeneric<T>(x: T) {}
}

@objc
public class BaseCPublic {
  public init(fromBaseC: TagPC) {}

  public func baseCFunc(x: TagPC) {}

  public subscript(a: TagPC) -> Int { return 1 }

  public var baseCVarRW: TagPC
  public let baseCVarRO: TagPC

  public func colliding() {}
  public func collidingGeneric<T>(x: T) {}
}

public protocol ProtocolD {
  func colliding()
  func collidingGeneric<T>(x: T)
}

// NO_ERRORS_UP_TO_HERE

private class TestPrivateAD : BaseAPrivate, ProtocolD {
  #^TEST_PRIVATE_AD^#
}
fileprivate class TestFilePrivateAD : BaseAPrivate, ProtocolD {
  #^TEST_FILEPRIVATE_AD^#
  // Same as TEST_PRIVATE_AD.
}
class TestInternalAD : BaseAPrivate, ProtocolD {
  #^TEST_INTERNAL_AD^#
}
public class TestPublicAD : ProtocolAPrivate, ProtocolD {
  #^TEST_PUBLIC_AD^#
}

// TEST_PRIVATE_AD: Begin completions, 6 items
// TEST_PRIVATE_AD-DAG: Decl[InstanceMethod]/Super:         override func baseAFunc(x: TagPA) {|}; name=baseAFunc(x:)
// TEST_PRIVATE_AD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPA) -> Int {|}; name=subscript(:)
// TEST_PRIVATE_AD-DAG: Decl[InstanceVar]/Super:            override var baseAVarRW: TagPA; name=baseAVarRW
// TEST_PRIVATE_AD-DAG: Decl[InstanceMethod]/Super:         override func colliding() {|}; name=colliding()
// TEST_PRIVATE_AD-DAG: Decl[InstanceMethod]/Super:         override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_PRIVATE_AD-DAG: Decl[Constructor]/Super:            override init(fromBase: TagPA) {|}; name=init(fromBase:)

// TEST_INTERNAL_AD: Begin completions, 6 items
// TEST_INTERNAL_AD-DAG: Decl[InstanceMethod]/Super:         override func baseAFunc(x: TagPA) {|}; name=baseAFunc(x:)
// TEST_INTERNAL_AD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPA) -> Int {|}; name=subscript(:)
// TEST_INTERNAL_AD-DAG: Decl[InstanceVar]/Super:            override var baseAVarRW: TagPA; name=baseAVarRW
// TEST_INTERNAL_AD-DAG: Decl[InstanceMethod]/Super:         override func colliding() {|}; name=colliding()
// TEST_INTERNAL_AD-DAG: Decl[InstanceMethod]/Super:         override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_INTERNAL_AD-DAG: Decl[Constructor]/Super:            override init(fromBase: TagPA) {|}; name=init(fromBase:)

// TEST_PUBLIC_AD: Begin completions, 2 items
// TEST_PUBLIC_AD-DAG: Decl[InstanceMethod]/Super:         public func colliding() {|}; name=colliding()
// TEST_PUBLIC_AD-DAG: Decl[InstanceMethod]/Super:         public func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)

private class TestPrivateBD : BaseBInternal, ProtocolD {
  #^TEST_PRIVATE_BD^#
}
fileprivate class TestFilePrivateBD : BaseBInternal, ProtocolD {
  #^TEST_FILEPRIVATE_BD^#
  // Same as TEST_PRIVATE_BD.
}
class TestInternalBD : BaseBInternal, ProtocolD {
  #^TEST_INTERNAL_BD^#
}
public class TestPublicBD : BaseBInternal, ProtocolD {
  #^TEST_PUBLIC_BD^#
}

// TEST_PRIVATE_BD: Begin completions, 6 items
// TEST_PRIVATE_BD-DAG: Decl[InstanceMethod]/Super:         override func baseBFunc(x: TagPB) {|}; name=baseBFunc(x:)
// TEST_PRIVATE_BD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPB) -> Int {|}; name=subscript(:)
// TEST_PRIVATE_BD-DAG: Decl[InstanceVar]/Super:            override var baseBVarRW: TagPB; name=baseBVarRW
// TEST_PRIVATE_BD-DAG: Decl[InstanceMethod]/Super:         override func colliding() {|}; name=colliding()
// TEST_PRIVATE_BD-DAG: Decl[InstanceMethod]/Super:         override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_PRIVATE_BD-DAG: Decl[Constructor]/Super:            override init(fromBaseB: TagPB) {|}; name=init(fromBaseB:)

// TEST_INTERNAL_BD: Begin completions, 6 items
// TEST_INTERNAL_BD-DAG: Decl[InstanceMethod]/Super:         override func baseBFunc(x: TagPB) {|}; name=baseBFunc(x:)
// TEST_INTERNAL_BD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPB) -> Int {|}; name=subscript(:)
// TEST_INTERNAL_BD-DAG: Decl[InstanceVar]/Super:            override var baseBVarRW: TagPB; name=baseBVarRW
// TEST_INTERNAL_BD-DAG: Decl[InstanceMethod]/Super:         override func colliding() {|}; name=colliding()
// TEST_INTERNAL_BD-DAG: Decl[InstanceMethod]/Super:         override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_INTERNAL_BD-DAG: Decl[Constructor]/Super:            override init(fromBaseB: TagPB) {|}; name=init(fromBaseB:)

// TEST_PUBLIC_BD: Begin completions, 6 items
// TEST_PUBLIC_BD-DAG: Decl[InstanceMethod]/Super:         override func baseBFunc(x: TagPB) {|}; name=baseBFunc(x:)
// TEST_PUBLIC_BD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPB) -> Int {|}; name=subscript(:)
// TEST_PUBLIC_BD-DAG: Decl[InstanceVar]/Super:            override var baseBVarRW: TagPB; name=baseBVarRW
// TEST_PUBLIC_BD-DAG: Decl[InstanceMethod]/Super:         public override func colliding() {|}; name=colliding()
// TEST_PUBLIC_BD-DAG: Decl[InstanceMethod]/Super:         public override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_PUBLIC_BD-DAG: Decl[Constructor]/Super:            override init(fromBaseB: TagPB) {|}; name=init(fromBaseB:)

private class TestPrivateCD : BaseCPublic, ProtocolD {
  #^TEST_PRIVATE_CD^#
}
fileprivate class TestFilePrivateCD : BaseCPublic, ProtocolD {
  #^TEST_FILEPRIVATE_CD^#
  // Same as TEST_PRIVATE_CD.
}
class TestInternalCD : BaseCPublic, ProtocolD {
  #^TEST_INTERNAL_CD^#
}
public class TestPublicCD : BaseCPublic, ProtocolD {
  #^TEST_PUBLIC_CD^#
}

// TEST_PRIVATE_CD: Begin completions, 6 items
// TEST_PRIVATE_CD-DAG: Decl[InstanceMethod]/Super:         override func baseCFunc(x: TagPC) {|}; name=baseCFunc(x:)
// TEST_PRIVATE_CD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPC) -> Int {|}; name=subscript(:)
// TEST_PRIVATE_CD-DAG: Decl[InstanceVar]/Super:            override var baseCVarRW: TagPC; name=baseCVarRW
// TEST_PRIVATE_CD-DAG: Decl[InstanceMethod]/Super:         override func colliding() {|}; name=colliding()
// TEST_PRIVATE_CD-DAG: Decl[InstanceMethod]/Super:         override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_PRIVATE_CD-DAG: Decl[Constructor]/Super:            override init(fromBaseC: TagPC) {|}; name=init(fromBaseC:)

// TEST_INTERNAL_CD: Begin completions, 6 items
// TEST_INTERNAL_CD-DAG: Decl[InstanceMethod]/Super:         override func baseCFunc(x: TagPC) {|}; name=baseCFunc(x:)
// TEST_INTERNAL_CD-DAG: Decl[Subscript]/Super:              override subscript(a: TagPC) -> Int {|}; name=subscript(:)
// TEST_INTERNAL_CD-DAG: Decl[InstanceVar]/Super:            override var baseCVarRW: TagPC; name=baseCVarRW
// TEST_INTERNAL_CD-DAG: Decl[InstanceMethod]/Super:         override func colliding() {|}; name=colliding()
// TEST_INTERNAL_CD-DAG: Decl[InstanceMethod]/Super:         override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_INTERNAL_CD-DAG: Decl[Constructor]/Super:            override init(fromBaseC: TagPC) {|}; name=init(fromBaseC:)

// TEST_PUBLIC_CD: Begin completions, 6 items
// TEST_PUBLIC_CD-DAG: Decl[InstanceMethod]/Super:         public override func baseCFunc(x: TagPC) {|}; name=baseCFunc(x:)
// TEST_PUBLIC_CD-DAG: Decl[Subscript]/Super:              public override subscript(a: TagPC) -> Int {|}; name=subscript(:)
// TEST_PUBLIC_CD-DAG: Decl[InstanceVar]/Super:            public override var baseCVarRW: TagPC; name=baseCVarRW
// TEST_PUBLIC_CD-DAG: Decl[InstanceMethod]/Super:         public override func colliding() {|}; name=colliding()
// TEST_PUBLIC_CD-DAG: Decl[InstanceMethod]/Super:         public override func collidingGeneric<T>(x: T) {|}; name=collidingGeneric(x:)
// TEST_PUBLIC_CD-DAG: Decl[Constructor]/Super:            public override init(fromBaseC: TagPC) {|}; name=init(fromBaseC:)

