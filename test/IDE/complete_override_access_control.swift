// RUN: sed -n -e '1,/NO_ERRORS_UP_TO_HERE$/ p' %s > %t_no_errors.swift
// RUN: %target-swift-frontend -parse -verify -disable-objc-attr-requires-foundation-module %t_no_errors.swift
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_ABC -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PRIVATE_ABC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_ABC -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_INTERNAL_ABC
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_ABC -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PUBLIC_ABC
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_DE -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PRIVATE_DE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_DE -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_INTERNAL_DE
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_DE -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PUBLIC_DE
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_ED -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PRIVATE_ED
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_ED -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_INTERNAL_ED
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_ED -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PUBLIC_ED
//
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PRIVATE_EF -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PRIVATE_EF
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_INTERNAL_EF -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_INTERNAL_EF
// RUN: %target-swift-ide-test -code-completion -source-filename %s -code-completion-token=TEST_PUBLIC_EF -code-completion-keywords=false | FileCheck %s -check-prefix=TEST_PUBLIC_EF

@objc
private class TagPA {}

@objc
class TagPB {}

@objc
public class TagPC {}

@objc
private protocol ProtocolAPrivate {
  init(fromProtocolA: TagPA)

  func protoAFunc(x: TagPA)
  optional func protoAFuncOptional(x: TagPA)

  @noreturn
  func protoAFuncWithAttr(x: TagPA)

  subscript(a: TagPA) -> Int { get }

  var protoAVarRW: TagPA { get set }
  var protoAVarRO: TagPA { get }
}

@objc
protocol ProtocolBInternal {
  init(fromProtocolB: TagPB)

  func protoBFunc(x: TagPB)
  optional func protoBFuncOptional(x: TagPB)

  @noreturn
  func protoBFuncWithBttr(x: TagPB)

  subscript(a: TagPB) -> Int { get }

  var protoBVarRW: TagPB { get set }
  var protoBVarRO: TagPB { get }
}

@objc
public protocol ProtocolCPublic {
  init(fromProtocolC: TagPC)

  func protoCFunc(x: TagPC)
  optional func protoCFuncOptional(x: TagPC)

  @noreturn
  func protoCFuncWithCttr(x: TagPC)

  subscript(a: TagPC) -> Int { get }

  var protoCVarRW: TagPC { get set }
  var protoCVarRO: TagPC { get }
}

private protocol ProtocolDPrivate {
  func colliding()
  func collidingGeneric<T>(x: T)
}

public protocol ProtocolEPublic {
  func colliding()
  func collidingGeneric<T>(x: T)
}

public protocol ProtocolFPublic {
  func colliding()
  func collidingGeneric<T>(x: T)
}

// NO_ERRORS_UP_TO_HERE

private class TestPrivateABC : ProtocolAPrivate, ProtocolBInternal, ProtocolCPublic {
  #^TEST_PRIVATE_ABC^#
}
class TestInternalABC : ProtocolAPrivate, ProtocolBInternal, ProtocolCPublic {
  #^TEST_INTERNAL_ABC^#
}
public class TestPublicABC : ProtocolAPrivate, ProtocolBInternal, ProtocolCPublic {
  #^TEST_PUBLIC_ABC^#
}

// TEST_PRIVATE_ABC: Begin completions, 12 items
// TEST_PRIVATE_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolA: TagPA) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private func protoAFunc(x: TagPA) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private func protoAFuncOptional(x: TagPA) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private @noreturn func protoAFuncWithAttr(x: TagPA) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolB: TagPB) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private func protoBFunc(x: TagPB) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private func protoBFuncOptional(x: TagPB) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private @noreturn func protoBFuncWithBttr(x: TagPB) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolC: TagPC) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private func protoCFunc(x: TagPC) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private func protoCFuncOptional(x: TagPC) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC-DAG: Decl[InstanceMethod]/Super: private @noreturn func protoCFuncWithCttr(x: TagPC) {|}{{; name=.+$}}
// TEST_PRIVATE_ABC: End completions

// TEST_INTERNAL_ABC: Begin completions, 12 items
// TEST_INTERNAL_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolA: TagPA) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: private func protoAFunc(x: TagPA) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: private func protoAFuncOptional(x: TagPA) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: private @noreturn func protoAFuncWithAttr(x: TagPA) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolB: TagPB) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: func protoBFunc(x: TagPB) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: func protoBFuncOptional(x: TagPB) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: @noreturn func protoBFuncWithBttr(x: TagPB) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolC: TagPC) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: func protoCFunc(x: TagPC) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: func protoCFuncOptional(x: TagPC) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC-DAG: Decl[InstanceMethod]/Super: @noreturn func protoCFuncWithCttr(x: TagPC) {|}{{; name=.+$}}
// TEST_INTERNAL_ABC: End completions

// TEST_PUBLIC_ABC: Begin completions, 12 items
// TEST_PUBLIC_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolA: TagPA) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: private func protoAFunc(x: TagPA) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: private func protoAFuncOptional(x: TagPA) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: private @noreturn func protoAFuncWithAttr(x: TagPA) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolB: TagPB) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: func protoBFunc(x: TagPB) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: func protoBFuncOptional(x: TagPB) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: @noreturn func protoBFuncWithBttr(x: TagPB) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[Constructor]/Super:    init(fromProtocolC: TagPC) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: public func protoCFunc(x: TagPC) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: public func protoCFuncOptional(x: TagPC) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC-DAG: Decl[InstanceMethod]/Super: public @noreturn func protoCFuncWithCttr(x: TagPC) {|}{{; name=.+$}}
// TEST_PUBLIC_ABC: End completions

private class TestPrivateDE : ProtocolDPrivate, ProtocolEPublic {
  #^TEST_PRIVATE_DE^#
}
class TestInternalDE : ProtocolDPrivate, ProtocolEPublic {
  #^TEST_INTERNAL_DE^#
}
public class TestPublicDE : ProtocolDPrivate, ProtocolEPublic {
  #^TEST_PUBLIC_DE^#
}

// FIXME: there should be no duplicates in the results below.

// TEST_PRIVATE_DE: Begin completions, 3 items
// TEST_PRIVATE_DE-DAG: Decl[InstanceMethod]/Super: private func colliding() {|}{{; name=.+$}}
// TEST_PRIVATE_DE-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_PRIVATE_DE-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

// TEST_INTERNAL_DE: Begin completions, 3 items
// TEST_INTERNAL_DE-DAG: Decl[InstanceMethod]/Super: func colliding() {|}{{; name=.+$}}
// TEST_INTERNAL_DE-DAG: Decl[InstanceMethod]/Super: func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_INTERNAL_DE-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

// TEST_PUBLIC_DE: Begin completions, 3 items
// TEST_PUBLIC_DE-DAG: Decl[InstanceMethod]/Super: public func colliding() {|}{{; name=.+$}}
// TEST_PUBLIC_DE-DAG: Decl[InstanceMethod]/Super: public func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_PUBLIC_DE-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

private class TestPrivateED : ProtocolEPublic, ProtocolDPrivate {
  #^TEST_PRIVATE_ED^#
}
class TestInternalED : ProtocolEPublic, ProtocolDPrivate {
  #^TEST_INTERNAL_ED^#
}
public class TestPublicED : ProtocolEPublic, ProtocolDPrivate {
  #^TEST_PUBLIC_ED^#
}

// FIXME: there should be no duplicates in the results below.

// TEST_PRIVATE_ED: Begin completions, 3 items
// TEST_PRIVATE_ED-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_PRIVATE_ED-DAG: Decl[InstanceMethod]/Super: private func colliding() {|}{{; name=.+$}}
// TEST_PRIVATE_ED-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

// TEST_INTERNAL_ED: Begin completions, 3 items
// TEST_INTERNAL_ED-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_INTERNAL_ED-DAG: Decl[InstanceMethod]/Super: func colliding() {|}{{; name=.+$}}
// TEST_INTERNAL_ED-DAG: Decl[InstanceMethod]/Super: func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

// TEST_PUBLIC_ED: Begin completions, 3 items
// TEST_PUBLIC_ED-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_PUBLIC_ED-DAG: Decl[InstanceMethod]/Super: public func colliding() {|}{{; name=.+$}}
// TEST_PUBLIC_ED-DAG: Decl[InstanceMethod]/Super: public func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

private class TestPrivateEF : ProtocolEPublic, ProtocolFPublic {
  #^TEST_PRIVATE_EF^#
}
class TestInternalEF : ProtocolEPublic, ProtocolFPublic {
  #^TEST_INTERNAL_EF^#
}
public class TestPublicEF : ProtocolEPublic, ProtocolFPublic {
  #^TEST_PUBLIC_EF^#
}

// FIXME: there should be no duplicates in the results below.

// TEST_PRIVATE_EF: Begin completions, 3 items
// TEST_PRIVATE_EF-DAG: Decl[InstanceMethod]/Super: private func colliding() {|}{{; name=.+$}}
// TEST_PRIVATE_EF-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_PRIVATE_EF-DAG: Decl[InstanceMethod]/Super: private func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

// TEST_INTERNAL_EF: Begin completions, 3 items
// TEST_INTERNAL_EF-DAG: Decl[InstanceMethod]/Super: func colliding() {|}{{; name=.+$}}
// TEST_INTERNAL_EF-DAG: Decl[InstanceMethod]/Super: func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_INTERNAL_EF-DAG: Decl[InstanceMethod]/Super: func collidingGeneric<T>(x: T) {|}{{; name=.+$}}

// TEST_PUBLIC_EF: Begin completions, 3 items
// TEST_PUBLIC_EF-DAG: Decl[InstanceMethod]/Super: public func colliding() {|}{{; name=.+$}}
// TEST_PUBLIC_EF-DAG: Decl[InstanceMethod]/Super: public func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
// TEST_PUBLIC_EF-DAG: Decl[InstanceMethod]/Super: public func collidingGeneric<T>(x: T) {|}{{; name=.+$}}
