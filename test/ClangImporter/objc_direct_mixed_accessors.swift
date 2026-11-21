// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -print-module -module-to-print=MixedAccessors \
// RUN:   -print-access -I %t/include -enable-objc-interop -source-filename x \
// RUN:   | %FileCheck %s --check-prefix=INTERFACE

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -enable-objc-interop -I %t/include %t/overrides.swift

// RUN: %target-swift-emit-silgen -enable-objc-interop -I %t/include \
// RUN:   %t/calls.swift | %FileCheck %s --check-prefix=SILGEN

// INTERFACE-LABEL: {{^}}open class MixedAccessors : Root {
// INTERFACE:       {{^}}  open var plain: CInt{{$}}
// INTERFACE:       {{^}}  public var directPropertyWithNonDirectGetter: CInt{{$}}
// INTERFACE:       {{^}}  public var nonDirectPropertyWithDirectGetter: CInt{{$}}
// INTERFACE:       {{^}}  public var nonDirectPropertyWithDirectSetter: CInt{{$}}
// INTERFACE:       {{^}}  public subscript(index: CInt) -> CInt{{$}}
// INTERFACE:       {{^}}  public subscript(key: Root!) -> CInt{{$}}
// INTERFACE:       {{^}}  open var ownerOfSharedSetter: CInt{{$}}
// INTERFACE-LABEL: {{^}}extension MixedAccessors {
// INTERFACE:       {{^}}  public var nonDirectSetter: CInt{{$}}
// INTERFACE-LABEL: {{^}}open class SubscriptSetterInExtension : Root {
// INTERFACE:       {{^}}  public subscript(index: CInt) -> CInt{{$}}

//--- include/module.modulemap
module MixedAccessors {
  header "MixedAccessors.h"
  export *
}

//--- include/MixedAccessors.h
__attribute__((objc_root_class))
@interface Root
@end

@interface MixedAccessors : Root
// Neither accessor is direct, so this property stays open.
@property (nonatomic) int plain;

// The direct properties in the class extension below reuse these non-direct
// accessors and synthesize only the other accessor, which is direct.
- (int)nonDirectGetter;
- (void)setNonDirectSetter:(int)value;
+ (int)classNonDirectGetter;

// The class extension below redeclares these as readwrite, so that either the
// setter is direct and the getter is not, or the other way around.
@property (nonatomic, readonly) int redeclaredWithDirectSetter;
@property (nonatomic, readonly, direct) int redeclaredWithNonDirectSetter;
@property (class, nonatomic, readonly) int classRedeclaredWithDirectSetter;

// A direct property whose getter is declared non-direct.
@property (nonatomic, direct) int directPropertyWithNonDirectGetter;
- (int)directPropertyWithNonDirectGetter;

// Non-direct properties with one accessor declared direct.
@property (nonatomic) int nonDirectPropertyWithDirectGetter;
- (int)nonDirectPropertyWithDirectGetter __attribute__((objc_direct));
@property (nonatomic) int nonDirectPropertyWithDirectSetter;
- (void)setNonDirectPropertyWithDirectSetter:(int)value __attribute__((objc_direct));

// Subscripts with one direct accessor.
- (int)objectAtIndexedSubscript:(int)index;
- (void)setObject:(int)object atIndexedSubscript:(int)index __attribute__((objc_direct));
- (int)objectForKeyedSubscript:(Root *)key __attribute__((objc_direct));
- (void)setObject:(int)object forKeyedSubscript:(Root *)key;

// The direct property in the class extension below reuses both of these
// accessors, so none of its accessors is direct. It's 'public', like any
// direct property, but not 'final'.
- (int)bothAccessorsNonDirect;
- (void)setBothAccessorsNonDirect:(int)value;

// The class extension below redeclares this as a direct readwrite property,
// but that property reuses the non-direct setter that the class extension
// declares, so it stays open.
@property (nonatomic, readonly) int redeclaredWithBothAccessorsNonDirect;

// The class extension below redeclares 'redeclaredWithSharedSetter' as a
// direct readwrite property, which reuses the non-direct setter of
// 'ownerOfSharedSetter', so both properties stay open.
@property (nonatomic, setter=setRedeclaredWithSharedSetter:) int ownerOfSharedSetter;
@property (nonatomic, readonly) int redeclaredWithSharedSetter;

// The class extension below redeclares this as readwrite, which makes this
// direct method its setter. Swift doesn't import that method at all, as the
// property's setter or as a method, so the property stays open.
@property (nonatomic, readonly) int redeclaredWithUnimportedDirectSetter;
- (void)setRedeclaredWithUnimportedDirectSetter:(int)value __attribute__((objc_direct));
@end

@interface MixedAccessors ()
@property (nonatomic, readwrite, direct) int nonDirectGetter;
@property (nonatomic, readwrite, direct) int nonDirectSetter;
@property (class, nonatomic, readwrite, direct) int classNonDirectGetter;
@property (nonatomic, readwrite, direct) int redeclaredWithDirectSetter;
@property (nonatomic, readwrite) int redeclaredWithNonDirectSetter;
@property (class, nonatomic, readwrite, direct) int classRedeclaredWithDirectSetter;
@property (nonatomic, readwrite, direct) int bothAccessorsNonDirect;
- (void)setRedeclaredWithBothAccessorsNonDirect:(int)value;
@property (nonatomic, readwrite, direct) int redeclaredWithBothAccessorsNonDirect;
@property (nonatomic, readwrite, direct) int redeclaredWithSharedSetter;
@property (nonatomic, readwrite) int redeclaredWithUnimportedDirectSetter;
@end

// A subscript whose direct setter is declared in a class extension.
@interface SubscriptSetterInExtension : Root
- (int)objectAtIndexedSubscript:(int)index;
@end

@interface SubscriptSetterInExtension ()
- (void)setObject:(int)object atIndexedSubscript:(int)index __attribute__((objc_direct));
@end

//--- overrides.swift
import MixedAccessors

class Subclass: MixedAccessors {
  override var plain: Int32 {
    get { return 0 }
    set {}
  }

  override var nonDirectGetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var nonDirectSetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override class var classNonDirectGetter: Int32 {
    // expected-error@-1 {{class property overrides a 'final' class property}}
    // expected-error@-2 {{overriding non-open class property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var redeclaredWithDirectSetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var redeclaredWithNonDirectSetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override class var classRedeclaredWithDirectSetter: Int32 {
    // expected-error@-1 {{class property overrides a 'final' class property}}
    // expected-error@-2 {{overriding non-open class property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var directPropertyWithNonDirectGetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var nonDirectPropertyWithDirectGetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var nonDirectPropertyWithDirectSetter: Int32 {
    // expected-error@-1 {{property overrides a 'final' property}}
    // expected-error@-2 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override subscript(index: Int32) -> Int32 {
    // expected-error@-1 {{subscript overrides a 'final' subscript}}
    // expected-error@-2 {{overriding non-open subscript outside of its defining module}}
    get { return 0 }
    set {}
  }

  override subscript(key: Root!) -> Int32 {
    // expected-error@-1 {{subscript overrides a 'final' subscript}}
    // expected-error@-2 {{overriding non-open subscript outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var bothAccessorsNonDirect: Int32 {
    // expected-error@-1 {{overriding non-open property outside of its defining module}}
    get { return 0 }
    set {}
  }

  override var redeclaredWithBothAccessorsNonDirect: Int32 {
    get { return 0 }
    set {}
  }

  override var ownerOfSharedSetter: Int32 {
    get { return 0 }
    set {}
  }

  override var redeclaredWithUnimportedDirectSetter: Int32 {
    return 0
  }
}

// Overriding both properties that share a setter in one class would give that
// class two setters with the same selector.
class SharedSetterSubclass: MixedAccessors {
  override var redeclaredWithSharedSetter: Int32 {
    get { return 0 }
    set {}
  }
}

class SubscriptSetterInExtensionSubclass: SubscriptSetterInExtension {
  override subscript(index: Int32) -> Int32 {
    // expected-error@-1 {{subscript overrides a 'final' subscript}}
    // expected-error@-2 {{overriding non-open subscript outside of its defining module}}
    get { return 0 }
    set {}
  }
}

//--- calls.swift
import MixedAccessors

// Each accessor is called directly if it is direct and through objc_msgSend
// otherwise, regardless of the other accessor.

// SILGEN-LABEL: sil hidden [ossa] @$s5calls5plainyySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.plain!getter.foreign
// SILGEN:         objc_method {{.*}}#MixedAccessors.plain!setter.foreign
// SILGEN:       } // end sil function '$s5calls5plainyySo14MixedAccessorsCF'
func plain(_ object: MixedAccessors) {
  object.plain = object.plain
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls15nonDirectGetteryySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.nonDirectGetter!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsC15nonDirectGetters5Int32VvsTo :
// SILGEN:       } // end sil function '$s5calls15nonDirectGetteryySo14MixedAccessorsCF'
func nonDirectGetter(_ object: MixedAccessors) {
  object.nonDirectGetter = object.nonDirectGetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls15nonDirectSetteryySo14MixedAccessorsCF :
// SILGEN:         function_ref @$sSo14MixedAccessorsC15nonDirectSetters5Int32VvgTo :
// SILGEN:         objc_method {{.*}}#MixedAccessors.nonDirectSetter!setter.foreign
// SILGEN:       } // end sil function '$s5calls15nonDirectSetteryySo14MixedAccessorsCF'
func nonDirectSetter(_ object: MixedAccessors) {
  object.nonDirectSetter = object.nonDirectSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls20classNonDirectGetteryyF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.classNonDirectGetter!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsC20classNonDirectGetters5Int32VvsZTo :
// SILGEN:       } // end sil function '$s5calls20classNonDirectGetteryyF'
func classNonDirectGetter() {
  MixedAccessors.classNonDirectGetter = MixedAccessors.classNonDirectGetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls26redeclaredWithDirectSetteryySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.redeclaredWithDirectSetter!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsC26redeclaredWithDirectSetters5Int32VvsTo :
// SILGEN:       } // end sil function '$s5calls26redeclaredWithDirectSetteryySo14MixedAccessorsCF'
func redeclaredWithDirectSetter(_ object: MixedAccessors) {
  object.redeclaredWithDirectSetter = object.redeclaredWithDirectSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls29redeclaredWithNonDirectSetteryySo14MixedAccessorsCF :
// SILGEN:         function_ref @$sSo14MixedAccessorsC29redeclaredWithNonDirectSetters5Int32VvgTo :
// SILGEN:         objc_method {{.*}}#MixedAccessors.redeclaredWithNonDirectSetter!setter.foreign
// SILGEN:       } // end sil function '$s5calls29redeclaredWithNonDirectSetteryySo14MixedAccessorsCF'
func redeclaredWithNonDirectSetter(_ object: MixedAccessors) {
  object.redeclaredWithNonDirectSetter = object.redeclaredWithNonDirectSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls31classRedeclaredWithDirectSetteryyF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.classRedeclaredWithDirectSetter!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsC31classRedeclaredWithDirectSetters5Int32VvsZTo :
// SILGEN:       } // end sil function '$s5calls31classRedeclaredWithDirectSetteryyF'
func classRedeclaredWithDirectSetter() {
  MixedAccessors.classRedeclaredWithDirectSetter =
    MixedAccessors.classRedeclaredWithDirectSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls33directPropertyWithNonDirectGetteryySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.directPropertyWithNonDirectGetter!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsC33directPropertyWithNonDirectGetters5Int32VvsTo :
// SILGEN:       } // end sil function '$s5calls33directPropertyWithNonDirectGetteryySo14MixedAccessorsCF'
func directPropertyWithNonDirectGetter(_ object: MixedAccessors) {
  object.directPropertyWithNonDirectGetter =
    object.directPropertyWithNonDirectGetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls021nonDirectPropertyWithC6GetteryySo14MixedAccessorsCF :
// SILGEN:         function_ref @$sSo14MixedAccessorsC021nonDirectPropertyWithD6Getters5Int32VvgTo :
// SILGEN:         objc_method {{.*}}#MixedAccessors.nonDirectPropertyWithDirectGetter!setter.foreign
// SILGEN:       } // end sil function '$s5calls021nonDirectPropertyWithC6GetteryySo14MixedAccessorsCF'
func nonDirectPropertyWithDirectGetter(_ object: MixedAccessors) {
  object.nonDirectPropertyWithDirectGetter =
    object.nonDirectPropertyWithDirectGetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls021nonDirectPropertyWithC6SetteryySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.nonDirectPropertyWithDirectSetter!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsC021nonDirectPropertyWithD6Setters5Int32VvsTo :
// SILGEN:       } // end sil function '$s5calls021nonDirectPropertyWithC6SetteryySo14MixedAccessorsCF'
func nonDirectPropertyWithDirectSetter(_ object: MixedAccessors) {
  object.nonDirectPropertyWithDirectSetter =
    object.nonDirectPropertyWithDirectSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls16indexedSubscriptyySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.subscript!getter.foreign
// SILGEN:         function_ref @$sSo14MixedAccessorsCys5Int32VADcisTo :
// SILGEN:       } // end sil function '$s5calls16indexedSubscriptyySo14MixedAccessorsCF'
func indexedSubscript(_ object: MixedAccessors) {
  object[0] = object[1]
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls14keyedSubscriptyySo14MixedAccessorsC_So4RootCtF :
// SILGEN:         function_ref @$sSo14MixedAccessorsCys5Int32VSo4RootCSgcigTo :
// SILGEN:         objc_method {{.*}}#MixedAccessors.subscript!setter.foreign
// SILGEN:       } // end sil function '$s5calls14keyedSubscriptyySo14MixedAccessorsC_So4RootCtF'
func keyedSubscript(_ object: MixedAccessors, _ key: Root) {
  object[key] = object[key]
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls22bothAccessorsNonDirectyySo05MixedC0CF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.bothAccessorsNonDirect!getter.foreign
// SILGEN:         objc_method {{.*}}#MixedAccessors.bothAccessorsNonDirect!setter.foreign
// SILGEN:       } // end sil function '$s5calls22bothAccessorsNonDirectyySo05MixedC0CF'
func bothAccessorsNonDirect(_ object: MixedAccessors) {
  object.bothAccessorsNonDirect = object.bothAccessorsNonDirect
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls36redeclaredWithBothAccessorsNonDirectyySo05MixedE0CF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.redeclaredWithBothAccessorsNonDirect!getter.foreign
// SILGEN:         objc_method {{.*}}#MixedAccessors.redeclaredWithBothAccessorsNonDirect!setter.foreign
// SILGEN:       } // end sil function '$s5calls36redeclaredWithBothAccessorsNonDirectyySo05MixedE0CF'
func redeclaredWithBothAccessorsNonDirect(_ object: MixedAccessors) {
  object.redeclaredWithBothAccessorsNonDirect =
    object.redeclaredWithBothAccessorsNonDirect
}

// Both properties use the same setter method.
// SILGEN-LABEL: sil hidden [ossa] @$s5calls12sharedSetteryySo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.ownerOfSharedSetter!getter.foreign
// SILGEN:         objc_method {{.*}}#MixedAccessors.ownerOfSharedSetter!setter.foreign
// SILGEN:         objc_method {{.*}}#MixedAccessors.redeclaredWithSharedSetter!getter.foreign
// SILGEN:         objc_method {{.*}}#MixedAccessors.{{ownerOfSharedSetter|redeclaredWithSharedSetter}}!setter.foreign
// SILGEN:       } // end sil function '$s5calls12sharedSetteryySo14MixedAccessorsCF'
func sharedSetter(_ object: MixedAccessors) {
  object.ownerOfSharedSetter = object.ownerOfSharedSetter
  object.redeclaredWithSharedSetter = object.redeclaredWithSharedSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls36redeclaredWithUnimportedDirectSetterys5Int32VSo14MixedAccessorsCF :
// SILGEN:         objc_method {{.*}}#MixedAccessors.redeclaredWithUnimportedDirectSetter!getter.foreign
// SILGEN:       } // end sil function '$s5calls36redeclaredWithUnimportedDirectSetterys5Int32VSo14MixedAccessorsCF'
func redeclaredWithUnimportedDirectSetter(_ object: MixedAccessors) -> Int32 {
  return object.redeclaredWithUnimportedDirectSetter
}

// SILGEN-LABEL: sil hidden [ossa] @$s5calls26subscriptSetterInExtensionyySo09SubscriptcdE0CF :
// SILGEN:         objc_method {{.*}}#SubscriptSetterInExtension.subscript!getter.foreign
// SILGEN:         function_ref @$sSo26SubscriptSetterInExtensionCys5Int32VADcisTo :
// SILGEN:       } // end sil function '$s5calls26subscriptSetterInExtensionyySo09SubscriptcdE0CF'
func subscriptSetterInExtension(_ object: SubscriptSetterInExtension) {
  object[0] = object[1]
}
