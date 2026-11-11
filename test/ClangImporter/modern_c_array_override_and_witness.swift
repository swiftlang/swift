// Tests @objc witnesses and subclasses with C arrays.

// REQUIRES: objc_interop
// REQUIRES: swift_feature_ModernImportedCArrays
// REQUIRES: swift_feature_ObjCAttrAllowsCArrays

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Library with the modern projection visible, client with the legacy projection
// RUN: %target-swift-frontend -emit-module -parse-as-library %t/lib.swift -module-name Lib -o %t/Lib.swiftmodule -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -DMODERN_C_ARRAYS -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/client.swift -I %t -target %target-has-inline-array-triple -enable-experimental-feature ObjCAttrAllowsCArrays -verify-additional-prefix modern-lib-
// RUN: %target-swift-frontend -emit-ir %t/lowering.swift -I %t -target %target-has-inline-array-triple -DLIB_MODERN_C_ARRAYS -o %t/lowering.ll -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %FileCheck %s --input-file %t/lowering.ll

// Library with the legacy projection visible, client with the modern projection
// RUN: %target-swift-frontend -emit-module -parse-as-library %t/lib.swift -module-name Lib -o %t/Lib.swiftmodule -target %target-has-inline-array-triple -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/client.swift -I %t -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -enable-experimental-feature ObjCAttrAllowsCArrays -verify-additional-prefix legacy-lib-
// RUN: %target-swift-frontend -emit-ir %t/lowering.swift -I %t -enable-experimental-feature ModernImportedCArrays -target %target-has-inline-array-triple -o %t/lowering.ll -enable-experimental-feature ObjCAttrAllowsCArrays
// RUN: %FileCheck %s --input-file %t/lowering.ll

//--- lib.swift
import Foundation

#if MODERN_C_ARRAYS
public typealias LibArray = InlineArray<4, Int32>
#else
public typealias LibArray = (Int32, Int32, Int32, Int32)
#endif

@objc public protocol Q {
  func take(_ a: UnsafeMutablePointer<LibArray>)
  var property: UnsafeMutablePointer<LibArray>? { get set }
  subscript(_ a: UnsafeMutablePointer<LibArray>) -> Int { get }
  func give() -> UnsafeMutablePointer<LibArray>
}

@objc open class Base: NSObject {
  @objc open func take(_ a: UnsafeMutablePointer<LibArray>) {}
  @objc open var property: UnsafeMutablePointer<LibArray>?
  @objc open subscript(_ a: UnsafeMutablePointer<LibArray>) -> Int { 0 }
  @objc open func give() -> UnsafeMutablePointer<LibArray> { fatalError() }
}

// Function types with pointers to C arrays in their parameters or results.
@objc public protocol CallbackQ {
  func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<LibArray>) -> Void)
  func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<LibArray>) -> Void)
  var callback: (@convention(c) (UnsafeMutablePointer<LibArray>) -> Void)? { get set }
  func giveCallback() -> @convention(c) (UnsafeMutablePointer<LibArray>) -> Void
}

@objc open class CallbackBase: NSObject {
  @objc open func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<LibArray>) -> Void) {}
  @objc open func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<LibArray>) -> Void) {}
  @objc open var callback: (@convention(c) (UnsafeMutablePointer<LibArray>) -> Void)?
  @objc open func giveCallback() -> @convention(c) (UnsafeMutablePointer<LibArray>) -> Void { fatalError() }
}

//--- client.swift
import Foundation
import Lib

typealias TupleArray = (Int32, Int32, Int32, Int32)
typealias InlineCArray = InlineArray<4, Int32>

class ConformerTuple: NSObject, Q {
  // expected-modern-lib-error@-1 {{type 'ConformerTuple' does not conform to protocol 'Q'}}
  // expected-modern-lib-note@-2 {{add stubs for conformance}}
  func take(_ a: UnsafeMutablePointer<TupleArray>) {}
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<TupleArray>) -> ()'}}
  var property: UnsafeMutablePointer<TupleArray>?
  // expected-modern-lib-note@-1 {{candidate has non-matching type 'UnsafeMutablePointer<TupleArray>?'}}
  subscript(_ a: UnsafeMutablePointer<TupleArray>) -> Int { 1 }
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<TupleArray>) -> Int'}}
  func give() -> UnsafeMutablePointer<TupleArray> { fatalError() }
  // expected-modern-lib-note@-1 {{candidate has non-matching type '() -> UnsafeMutablePointer<TupleArray>'}}
}

class ConformerInline: NSObject, Q {
  // expected-legacy-lib-error@-1 {{type 'ConformerInline' does not conform to protocol 'Q'}}
  // expected-legacy-lib-note@-2 {{add stubs for conformance}}
  func take(_ a: UnsafeMutablePointer<InlineCArray>) {}
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<InlineCArray>) -> ()'}}
  var property: UnsafeMutablePointer<InlineCArray>?
  // expected-legacy-lib-note@-1 {{candidate has non-matching type 'UnsafeMutablePointer<InlineCArray>?'}}
  subscript(_ a: UnsafeMutablePointer<InlineCArray>) -> Int { 1 }
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<InlineCArray>) -> Int'}}
  func give() -> UnsafeMutablePointer<InlineCArray> { fatalError() }
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '() -> UnsafeMutablePointer<InlineCArray>'}}
}

class SubclassTuple: Base {
  override func take(_ a: UnsafeMutablePointer<TupleArray>) {}
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}
  override var property: UnsafeMutablePointer<TupleArray>? {
  // expected-modern-lib-error@-1 {{cannot override a property with type 'UnsafeMutablePointer<LibArray>?'}}
    get { nil }
    set {}
  }
  override subscript(_ a: UnsafeMutablePointer<TupleArray>) -> Int { 2 }
  // expected-modern-lib-error@-1 {{subscript does not override any subscript from its superclass}}
  override func give() -> UnsafeMutablePointer<TupleArray> { fatalError() }
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}

  // A new declaration can use either projection.
  @objc func new(_ a: UnsafeMutablePointer<TupleArray>) {}
}

class SubclassInline: Base {
  override func take(_ a: UnsafeMutablePointer<InlineCArray>) {}
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}
  override var property: UnsafeMutablePointer<InlineCArray>? {
  // expected-legacy-lib-error@-1 {{cannot override a property with type 'UnsafeMutablePointer<LibArray>?'}}
    get { nil }
    set {}
  }
  override subscript(_ a: UnsafeMutablePointer<InlineCArray>) -> Int { 2 }
  // expected-legacy-lib-error@-1 {{subscript does not override any subscript from its superclass}}
  override func give() -> UnsafeMutablePointer<InlineCArray> { fatalError() }
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}

  // A new declaration can use either projection.
  @objc func new(_ a: UnsafeMutablePointer<InlineCArray>) {}
}

class ExplicitConformerTuple: NSObject, Q {
  // expected-modern-lib-error@-1 {{type 'ExplicitConformerTuple' does not conform to protocol 'Q'}}
  // expected-modern-lib-note@-2 {{add stubs for conformance}}
  @objc func take(_ a: UnsafeMutablePointer<TupleArray>) {}
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<TupleArray>) -> ()'}}
  @objc var property: UnsafeMutablePointer<TupleArray>?
  // expected-modern-lib-note@-1 {{candidate has non-matching type 'UnsafeMutablePointer<TupleArray>?'}}
  @objc subscript(_ a: UnsafeMutablePointer<TupleArray>) -> Int { 1 }
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<TupleArray>) -> Int'}}
  @objc func give() -> UnsafeMutablePointer<TupleArray> { fatalError() }
  // expected-modern-lib-note@-1 {{candidate has non-matching type '() -> UnsafeMutablePointer<TupleArray>'}}
}

class ExplicitConformerInline: NSObject, Q {
  // expected-legacy-lib-error@-1 {{type 'ExplicitConformerInline' does not conform to protocol 'Q'}}
  // expected-legacy-lib-note@-2 {{add stubs for conformance}}
  @objc func take(_ a: UnsafeMutablePointer<InlineCArray>) {}
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<InlineCArray>) -> ()'}}
  @objc var property: UnsafeMutablePointer<InlineCArray>?
  // expected-legacy-lib-note@-1 {{candidate has non-matching type 'UnsafeMutablePointer<InlineCArray>?'}}
  @objc subscript(_ a: UnsafeMutablePointer<InlineCArray>) -> Int { 1 }
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(UnsafeMutablePointer<InlineCArray>) -> Int'}}
  @objc func give() -> UnsafeMutablePointer<InlineCArray> { fatalError() }
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '() -> UnsafeMutablePointer<InlineCArray>'}}
}

class ExplicitSubclassTuple: Base {
  @objc override func take(_ a: UnsafeMutablePointer<TupleArray>) {}
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}
  @objc override var property: UnsafeMutablePointer<TupleArray>? {
  // expected-modern-lib-error@-1 {{cannot override a property with type 'UnsafeMutablePointer<LibArray>?'}}
    get { nil }
  // expected-modern-lib-error@-1 {{getter for 'property' with Objective-C selector 'property' conflicts with getter for 'property' from superclass 'Base' with the same Objective-C selector}}
    set {}
  // expected-modern-lib-error@-1 {{setter for 'property' with Objective-C selector 'setProperty:' conflicts with setter for 'property' from superclass 'Base' with the same Objective-C selector}}
  }
  @objc override subscript(_ a: UnsafeMutablePointer<TupleArray>) -> Int { 2 }
  // expected-modern-lib-error@-1 {{subscript does not override any subscript from its superclass}}
  // expected-modern-lib-error@-2 {{subscript getter with Objective-C selector 'objectForKeyedSubscript:' conflicts with subscript getter from superclass 'Base' with the same Objective-C selector}}
  @objc override func give() -> UnsafeMutablePointer<TupleArray> { fatalError() }
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}
}

class ExplicitSubclassInline: Base {
  @objc override func take(_ a: UnsafeMutablePointer<InlineCArray>) {}
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}
  @objc override var property: UnsafeMutablePointer<InlineCArray>? {
  // expected-legacy-lib-error@-1 {{cannot override a property with type 'UnsafeMutablePointer<LibArray>?'}}
    get { nil }
  // expected-legacy-lib-error@-1 {{getter for 'property' with Objective-C selector 'property' conflicts with getter for 'property' from superclass 'Base' with the same Objective-C selector}}
    set {}
  // expected-legacy-lib-error@-1 {{setter for 'property' with Objective-C selector 'setProperty:' conflicts with setter for 'property' from superclass 'Base' with the same Objective-C selector}}
  }
  @objc override subscript(_ a: UnsafeMutablePointer<InlineCArray>) -> Int { 2 }
  // expected-legacy-lib-error@-1 {{subscript does not override any subscript from its superclass}}
  // expected-legacy-lib-error@-2 {{subscript getter with Objective-C selector 'objectForKeyedSubscript:' conflicts with subscript getter from superclass 'Base' with the same Objective-C selector}}
  @objc override func give() -> UnsafeMutablePointer<InlineCArray> { fatalError() }
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}
}

class CallbackConformerTuple: NSObject, CallbackQ {
  // expected-modern-lib-error@-1 {{type 'CallbackConformerTuple' does not conform to protocol 'CallbackQ'}}
  // expected-modern-lib-note@-2 {{add stubs for conformance}}
  func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void) {}
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void) -> ()'}}
  func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<TupleArray>) -> Void) {}
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(@convention(block) (UnsafeMutablePointer<TupleArray>) -> Void) -> ()'}}
  var callback: (@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void)?
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void)?'}}
  func giveCallback() -> @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void { fatalError() }
  // expected-modern-lib-note@-1 {{candidate has non-matching type '() -> @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void'}}
}

class CallbackConformerInline: NSObject, CallbackQ {
  // expected-legacy-lib-error@-1 {{type 'CallbackConformerInline' does not conform to protocol 'CallbackQ'}}
  // expected-legacy-lib-note@-2 {{add stubs for conformance}}
  func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void) {}
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void) -> ()'}}
  func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<InlineCArray>) -> Void) {}
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(@convention(block) (UnsafeMutablePointer<InlineCArray>) -> Void) -> ()'}}
  var callback: (@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void)?
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void)?'}}
  func giveCallback() -> @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void { fatalError() }
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '() -> @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void'}}
}

class CallbackSubclassTuple: CallbackBase {
  override func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void) {}
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}
  override func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<TupleArray>) -> Void) {}
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}
  override var callback: (@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void)? {
  // expected-modern-lib-error@-1 {{cannot override a property with type '(@convention(c) (UnsafeMutablePointer<LibArray>) -> Void)?'}}
    get { nil }
    set {}
  }
  override func giveCallback() -> @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void { fatalError() }
  // expected-modern-lib-error@-1 {{method does not override any method from its superclass}}
}

class CallbackSubclassInline: CallbackBase {
  override func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void) {}
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}
  override func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<InlineCArray>) -> Void) {}
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}
  override var callback: (@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void)? {
  // expected-legacy-lib-error@-1 {{cannot override a property with type '(@convention(c) (UnsafeMutablePointer<LibArray>) -> Void)?'}}
    get { nil }
    set {}
  }
  override func giveCallback() -> @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void { fatalError() }
  // expected-legacy-lib-error@-1 {{method does not override any method from its superclass}}
}

class ExplicitCallbackConformerTuple: NSObject, CallbackQ {
  // expected-modern-lib-error@-1 {{type 'ExplicitCallbackConformerTuple' does not conform to protocol 'CallbackQ'}}
  // expected-modern-lib-note@-2 {{add stubs for conformance}}
  @objc func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void) {}
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void) -> ()'}}
  @objc func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<TupleArray>) -> Void) {}
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(@convention(block) (UnsafeMutablePointer<TupleArray>) -> Void) -> ()'}}
  @objc var callback: (@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void)?
  // expected-modern-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<TupleArray>) -> Void)?'}}
  @objc func giveCallback() -> @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void { fatalError() }
  // expected-modern-lib-note@-1 {{candidate has non-matching type '() -> @convention(c) (UnsafeMutablePointer<TupleArray>) -> Void'}}
}

class ExplicitCallbackConformerInline: NSObject, CallbackQ {
  // expected-legacy-lib-error@-1 {{type 'ExplicitCallbackConformerInline' does not conform to protocol 'CallbackQ'}}
  // expected-legacy-lib-note@-2 {{add stubs for conformance}}
  @objc func takeCallback(_ f: @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void) {}
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void) -> ()'}}
  @objc func takeBlock(_ f: @convention(block) (UnsafeMutablePointer<InlineCArray>) -> Void) {}
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(@convention(block) (UnsafeMutablePointer<InlineCArray>) -> Void) -> ()'}}
  @objc var callback: (@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void)?
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '(@convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void)?'}}
  @objc func giveCallback() -> @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void { fatalError() }
  // expected-legacy-lib-note@-1 {{candidate has non-matching type '() -> @convention(c) (UnsafeMutablePointer<InlineCArray>) -> Void'}}
}

//--- lowering.swift
import Foundation
import Lib

#if LIB_MODERN_C_ARRAYS
typealias ClientArray = InlineArray<4, Int32>
#else
typealias ClientArray = (Int32, Int32, Int32, Int32)
#endif

class Conformer: NSObject, Q {
  func take(_ a: UnsafeMutablePointer<ClientArray>) {}
  var property: UnsafeMutablePointer<ClientArray>?
  subscript(_ a: UnsafeMutablePointer<ClientArray>) -> Int { 1 }
  func give() -> UnsafeMutablePointer<ClientArray> { fatalError() }
}

// CHECK-DAG: @_IVARS__TtC{{.*}}9Conformer = {{.*}} ptr @.str.8.property, ptr @".str.{{[0-9]+}}.^[4i]"
// CHECK-DAG: @_PROPERTIES__TtC{{.*}}9Conformer = {{.*}} ptr @.str.8.property, ptr @".str.{{[0-9]+}}.T^[4i],N,Vproperty"

class Subclass: Base {
  override func take(_ a: UnsafeMutablePointer<ClientArray>) {}
  override var property: UnsafeMutablePointer<ClientArray>? {
    get { nil }
    set {}
  }
  override subscript(_ a: UnsafeMutablePointer<ClientArray>) -> Int { 2 }
  override func give() -> UnsafeMutablePointer<ClientArray> { fatalError() }
}

func use(_ q: Q, _ base: Base, _ pointer: UnsafeMutablePointer<ClientArray>) {
  q.take(pointer)
  base.take(pointer)
  _ = q[pointer] + base[pointer]
}

// An explicit @objc doesn't make an override or witness a new declaration.

class ExplicitConformer: NSObject, Q {
  @objc func take(_ a: UnsafeMutablePointer<ClientArray>) {}
  @objc var property: UnsafeMutablePointer<ClientArray>?
  @objc subscript(_ a: UnsafeMutablePointer<ClientArray>) -> Int { 1 }
  @objc func give() -> UnsafeMutablePointer<ClientArray> { fatalError() }
}

// CHECK-DAG: @_IVARS__TtC{{.*}}17ExplicitConformer = {{.*}} ptr @.str.8.property, ptr @".str.{{[0-9]+}}.^[4i]"
// CHECK-DAG: @_PROPERTIES__TtC{{.*}}17ExplicitConformer = {{.*}} ptr @.str.8.property, ptr @".str.{{[0-9]+}}.T^[4i],N,Vproperty"

class ExplicitSubclass: Base {
  @objc override func take(_ a: UnsafeMutablePointer<ClientArray>) {}
  @objc override var property: UnsafeMutablePointer<ClientArray>? {
    get { nil }
    set {}
  }
  @objc override subscript(_ a: UnsafeMutablePointer<ClientArray>) -> Int { 2 }
  @objc override func give() -> UnsafeMutablePointer<ClientArray> { fatalError() }
}
