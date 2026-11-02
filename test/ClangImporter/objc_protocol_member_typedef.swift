// REQUIRES: objc_interop

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -import-objc-header %S/Inputs/objc_protocol_member_typedef.h -typecheck -verify -verify-ignore-unrelated -verify-additional-file %S/Inputs/objc_protocol_member_typedef.h %s

// RUN: echo '#include "objc_protocol_member_typedef.h"' > %t.m
// RUN: %target-swift-ide-test -source-filename %s -print-header -header-to-print %S/Inputs/objc_protocol_member_typedef.h -import-objc-header %S/Inputs/objc_protocol_member_typedef.h --cc-args %target-cc-options -fsyntax-only %t.m -I %S/Inputs > %t.txt
// RUN: %FileCheck -check-prefix=ALIAS %s < %t.txt
// RUN: %FileCheck -check-prefix=PROTOCOL %s < %t.txt
// RUN: %FileCheck -check-prefix=USES %s < %t.txt

// The typedef is imported as a typealias in an extension of the protocol.
// ALIAS:      extension Parent {
// ALIAS-NEXT:   typealias Child = any ParentChildProtocol
// ALIAS-NEXT: }

// References to the typedef use its underlying type, since the typealias is
// only available through the protocol.
// PROTOCOL:      protocol Parent : NSObjectProtocol {
// PROTOCOL-NEXT:   var child: any ParentChildProtocol { get }
// PROTOCOL-NEXT:   func adoptChild(_ child: any ParentChildProtocol)
// PROTOCOL-NEXT: }

// USES-DAG: func ParentUseChild(_ child: any ParentChildProtocol)
// USES-DAG: var heldChild: (any ParentChildProtocol)?

func useNestedName(_ parent: any Parent, holder: ParentHolder) {
  let child: Parent.Child = parent.child
  child.play()
  parent.adoptChild(child)
  ParentUseChild(child)
  holder.heldChild = child

  let erased: any ParentChildProtocol = child
  let _: Parent.Child = erased
}

class ConcreteParent: NSObject, Parent {
  var child: Parent.Child { fatalError() }
  func adoptChild(_ child: Parent.Child) {}
}

func useThroughConformingType(_ parent: ConcreteParent) {
  let _: ConcreteParent.Child = parent.child
}

func useGeneric<P: Parent>(_ parent: P) -> P.Child {
  return parent.child
}

func useObjCName(_ child: ParentChild) {} // expected-error {{'ParentChild' has been renamed to 'Parent.Child'}}
