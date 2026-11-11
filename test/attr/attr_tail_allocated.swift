// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -typecheck -verify -package-name pkg %t/main.swift -enable-experimental-feature Embedded -enable-experimental-feature TailAllocatedAttribute
// RUN: %if swift_feature_ParserASTGen %{ %target-swift-frontend -target %target-cpu-apple-macos14 -typecheck -verify -package-name pkg %t/main.swift -enable-experimental-feature Embedded -enable-experimental-feature TailAllocatedAttribute -enable-experimental-feature ParserASTGen %}
// RUN: not %target-swift-frontend -target %target-cpu-apple-macos14 -typecheck %t/main.swift -enable-experimental-feature TailAllocatedAttribute 2>&1 | %FileCheck %s --check-prefix=NOT-EMBEDDED
// RUN: not %target-swift-frontend -target %target-cpu-apple-macos14 -typecheck %t/main.swift -enable-experimental-feature Embedded 2>&1 | %FileCheck %s --check-prefix=NO-FEATURE
// RUN: not %target-swift-frontend -target %target-cpu-apple-macos14 -typecheck %t/malformed.swift -enable-experimental-feature Embedded -enable-experimental-feature TailAllocatedAttribute 2>&1 | %FileCheck %s --check-prefix=MALFORMED
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -emit-silgen %t/silgen.swift -module-name m -enable-experimental-feature Embedded -enable-experimental-feature TailAllocatedAttribute -enable-experimental-feature BuiltinModule | %FileCheck %s --check-prefix=SILGEN
// RUN: not %target-swift-frontend -target %target-cpu-apple-macos14 -emit-silgen %t/silgen.swift -module-name m -enable-experimental-feature Embedded -enable-experimental-feature TailAllocatedAttribute -enable-experimental-feature BuiltinModule -D WRONG 2>&1 | %FileCheck %s --check-prefix=WRONG

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_TailAllocatedAttribute
// REQUIRES: swift_feature_BuiltinModule

// NOT-EMBEDDED: error: '@_tailAllocated' is only supported in Embedded Swift
// NO-FEATURE: error: '@_tailAllocated' attribute is only valid when experimental feature TailAllocatedAttribute is enabled

// Malformed arguments are checked with the C++ parser only: like its other
// attribute conversions, ASTGen doesn't diagnose them yet.
// MALFORMED: error: expected at least one element type after the counts property in '@_tailAllocated'
// MALFORMED: error: expected 'counts:' and a property name in '@_tailAllocated'

//--- main.swift
import _Concurrency

// The counts are a stored or computed property, possibly inherited: an Int
// for one element type, a tuple of Ints for several.
@_tailAllocated(counts: _tailCount, Element)
open class Base<Element> {
  @usableFromInline final var _tailCount: Int { 0 }
}
final class Sub: Base<UInt8> {}

@_tailAllocated(counts: storedCounts, UInt8, Int)
final class Stored {
  var storedCounts: (Int, Int) = (0, 0)
}

// Instances must be allocated with Builtin.allocWithTailElems; `self.init` and
// `super.init` delegation isn't an allocation.
@_tailAllocated(counts: count, UInt8)
class InitAllocated {
  final var count = 0
  init() {}
  convenience init(x: Int) { self.init() }
}
final class InitAllocatedSub: InitAllocated {
  override init() { super.init() }
}
func allocateThroughInitializers() {
  _ = InitAllocated() // expected-error {{'InitAllocated' has '@_tailAllocated' and must be allocated with 'Builtin.allocWithTailElems', not an initializer}}
  _ = InitAllocated.init(x:) // expected-error {{'InitAllocated' has '@_tailAllocated'}}
  _ = InitAllocatedSub() // expected-error {{'InitAllocatedSub' has '@_tailAllocated'}}
}

@_tailAllocated(counts: missing, Int) // expected-error {{'@_tailAllocated' counts property 'missing' must be a 'final' instance property of 'Missing' of type 'Int' (one 'Int' per element type), not 'async' or 'throws'}}
final class Missing {}

@_tailAllocated(counts: wrongArity, Int, UInt8) // expected-error {{counts property 'wrongArity' must be a 'final' instance property of 'WrongArity' of type '(Int, Int)'}}
final class WrongArity {
  var wrongArity: Int { 0 }
}

// A subclass could override a non-final counts property.
@_tailAllocated(counts: overridable, Int) // expected-error {{counts property 'overridable' must be a 'final' instance property of 'Overridable'}}
class Overridable {
  var overridable: Int { 0 }
}

@_tailAllocated(counts: hidden, Int) // expected-error {{'@_tailAllocated' counts property 'hidden' must be 'public' or '@usableFromInline' because 'Visible' is public}}
public final class Visible {
  var hidden: Int { 0 }
}

@_tailAllocated(counts: hiddenFromPackage, Int) // expected-error {{'@_tailAllocated' counts property 'hiddenFromPackage' must be 'package', 'public' or '@usableFromInline' because 'PackageVisible' is package}}
package final class PackageVisible {
  var hiddenFromPackage: Int { 0 }
}

@_tailAllocated(counts: _tailCount, UInt8) // expected-error {{'@_tailAllocated' cannot be used on 'Redeclared' because its superclass 'Base' already has it}}
final class Redeclared: Base<Int> {}

@_tailAllocated(counts: count, Int) // expected-error {{'@_tailAllocated' cannot be used on an actor}}
actor AnActor {
  var count: Int { 0 }
}

@_tailAllocated(counts: count, NoSuchType) // expected-error {{cannot find type 'NoSuchType' in scope}}
final class BadType {
  var count: Int { 0 }
}

@_tailAllocated(counts: count, Int) // expected-error {{'@_tailAllocated' may only be used on 'class' declarations}}
struct NotAClass {
  var count: Int { 0 }
}

//--- malformed.swift
@_tailAllocated(counts: count)
final class NoTypes {
  var count: Int { 0 }
}

@_tailAllocated(Int)
final class NoCounts {
  var count: Int { 0 }
}

//--- silgen.swift
import Builtin

// The deallocating deinit reads the counts before the destroyer and names the
// tail elements at the dealloc_ref. The counts property can be inherited from
// a generic superclass; several counts are a tuple.
class Counted<T> {
  final var counts: (Int, Int) = (0, 0)
}
@_tailAllocated(counts: counts, UInt8, Int64)
final class TwoTails: Counted<UInt8> {}

// SILGEN-LABEL: sil hidden [ossa] @$e1m8TwoTailsCfD :
// SILGEN:         [[BASE:%.*]] = upcast {{%.*}} to $Counted<UInt8>
// SILGEN:         [[ADDR:%.*]] = ref_element_addr [[BASE]], #Counted.counts
// SILGEN:         ({{%.*}}, {{%.*}}) = destructure_tuple
// SILGEN:         [[BYTES:%.*]] = struct_extract {{%.*}}, #Int._value
// SILGEN:         [[WORDS:%.*]] = struct_extract {{%.*}}, #Int._value
// SILGEN:         apply {{%.*}}({{%.*}}) : $@convention(method) (@guaranteed TwoTails) -> @owned Builtin.NativeObject
// SILGEN:         dealloc_ref [tail_elems $UInt8 * [[BYTES]]] [tail_elems $Int64 * [[WORDS]]]
// SILGEN-LABEL: } // end sil function '$e1m8TwoTailsCfD'

#if WRONG
// WRONG: error: 'TwoTails' must be tail-allocated with its '@_tailAllocated' element types
func allocate(_ n: Int) -> TwoTails {
  Builtin.allocWithTailElems_1(TwoTails.self, n._builtinWordValue, UInt8.self)
}
#endif
