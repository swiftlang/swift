// RUN: %empty-directory(%t)


// First test: computed effects should be serialized if a module is _not_ compiled with -enable-library-evolution.

// RUN: %target-swift-frontend -parse-as-library -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module -DMODULE %s -O -Xllvm -sil-disable-pass=cmo -c -o module.o
// RUN: %target-sil-opt %t/Module.swiftmodule | %FileCheck --check-prefix=CHECK-FR-MODULE %s
// RUN: %target-swift-frontend -parse-as-library -I%t -module-name=Main -DMAIN %s -O -emit-sil | %FileCheck --check-prefix=CHECK-MAIN --check-prefix=CHECK-FR-MAIN %s

// Second test: computed effects should _not_ be serialized if a module is compiled with -enable-library-evolution.

// RUN: %target-swift-frontend -parse-as-library -emit-module -emit-module-path=%t/Module.swiftmodule -module-name=Module -DMODULE %s -O -enable-library-evolution -c -o module.o
// RUN: %target-sil-opt %t/Module.swiftmodule | %FileCheck --check-prefix=CHECK-LE-MODULE %s
// RUN: %target-swift-frontend -parse-as-library -I%t -module-name=Main -DMAIN %s -O -emit-sil | %FileCheck --check-prefix=CHECK-MAIN --check-prefix=CHECK-LE-MAIN %s

// REQUIRES: swift_in_compiler

#if MODULE

public final class X {
  @usableFromInline
  var i = 27

  public init() { }
}

// CHECK-FR-MODULE-LABEL: sil [canonical] @$s6Module17noInlineNoEffectsySiAA1XCF :
// CHECK-FR-MODULE-NEXT:  [%0: noescape **]
// CHECK-FR-MODULE-NEXT:  {{^[^[]}}
// CHECK-LE-MODULE-NOT: @$s6Module17noInlineNoEffectsySiAA1XCF
public func noInlineNoEffects(_ x: X) -> Int {
  return x.i
}

// CHECK-FR-MODULE-LABEL: sil [serialized] [noinline] [canonical] @$s6Module15inlineNoEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MODULE-NEXT:  [%0: noescape **]
// CHECK-FR-MODULE-NEXT:  {{^[^[]}}
// CHECK-LE-MODULE-LABEL: sil [serialized] [noinline] [canonical] @$s6Module15inlineNoEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-LE-MODULE-NEXT:  {{^[^[]}}
@inlinable
@inline(never)
public func inlineNoEffects(_ x: X) -> Int {
  return internalCallee(x)
}

// CHECK-FR-MODULE-LABEL: sil [canonical] @$s6Module14internalCalleeySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MODULE-NEXT:  [%0: noescape **]
// CHECK-FR-MODULE-NEXT:  {{^[^[]}}
// CHECK-LE-MODULE-LABEL: sil [canonical] @$s6Module14internalCalleeySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int{{$}}
@usableFromInline
func internalCallee(_ x: X) -> Int {
  return x.i
}

// CHECK-FR-MODULE-LABEL: sil [canonical] @$s6Module19noInlineWithEffectsySiAA1XCF :
// CHECK-FR-MODULE-NEXT:  [%0!: noescape **]
// CHECK-FR-MODULE-NEXT:  {{^[^[]}}
// CHECK-LE-MODULE-NOT: @$s6Module19noInlineWithEffectsySiAA1XCF
@_effects(notEscaping x.**)
public func noInlineWithEffects(_ x: X) -> Int {
  return x.i
}

// CHECK-FR-MODULE-LABEL: sil [serialized] [noinline] [canonical] @$s6Module17inlineWithEffectsySiAA1XCF :
// CHECK-FR-MODULE-NEXT:  [%0!: noescape **]
// CHECK-FR-MODULE-NEXT:  {{^[^[]}}
// CHECK-LE-MODULE-LABEL: sil [serialized] [noinline] [canonical] @$s6Module17inlineWithEffectsySiAA1XCF :
// CHECK-LE-MODULE-NEXT:  [%0!: noescape **]
// CHECK-LE-MODULE-NEXT:  {{^[^[]}}
@inlinable
@inline(never)
@_effects(notEscaping x.**)
public func inlineWithEffects(_ x: X) -> Int {
  return internalCallee(x)
}

// CHECK-FR-MODULE-LABEL: sil [serialized] [noinline] [canonical] @$s6Module12simpleInlineySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MODULE-NEXT:  [%0: noescape **]
// CHECK-FR-MODULE-NEXT:  {{^[^[]}}
// CHECK-LE-MODULE-LABEL: sil [serialized] [noinline] [canonical] @$s6Module12simpleInlineySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-LE-MODULE-NEXT:  {{^[^[]}}
@inlinable
@inline(never)
public func simpleInline(_ x: X) -> Int {
  return x.i
}

#else

import Module

// CHECK-MAIN-LABEL: sil [noinline] @$s4Main7callit1SiyF :
// CHECK-FR-MAIN:       alloc_ref [stack] $X
// CHECK-LE-MAIN:       alloc_ref $X
// CHECK-MAIN:       } // end sil function '$s4Main7callit1SiyF'
@inline(never)
public func callit1() -> Int {
  let x = X()
  return noInlineNoEffects(x)
}

// CHECK-FR-MAIN-LABEL: sil @$s6Module17noInlineNoEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int
// CHECK-FR-MAIN-NEXT:  [%0: noescape **]
// CHECK-FR-MAIN-NEXT:  {{^[^[]}}
// CHECK-LE-MAIN-LABEL: sil @$s6Module17noInlineNoEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int{{$}}

// CHECK-MAIN-LABEL: sil [noinline] @$s4Main7callit2SiyF :
// CHECK-FR-MAIN:       alloc_ref [stack] $X
// CHECK-LE-MAIN:       alloc_ref $X
// CHECK-MAIN:       } // end sil function '$s4Main7callit2SiyF'
@inline(never)
public func callit2() -> Int {
  let x = X()
  return inlineNoEffects(x)
}

// CHECK-FR-MAIN-LABEL: sil public_external [noinline] @$s6Module15inlineNoEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MAIN-NEXT:  [%0: noescape **]
// CHECK-FR-MAIN-NEXT:  {{^[^[]}}
// CHECK-LE-MAIN-LABEL: sil public_external [noinline] @$s6Module15inlineNoEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-LE-MAIN-NEXT:  {{^[^[]}}

// CHECK-MAIN-LABEL: sil [noinline] @$s4Main7callit3SiyF :
// CHECK-FR-MAIN:       alloc_ref [stack] $X
// CHECK-LE-MAIN:       = function_ref @$s6Module1XCACycfc
// CHECK-MAIN:       } // end sil function '$s4Main7callit3SiyF'
@inline(never)
public func callit3() -> Int {
  let x = X()
  return noInlineWithEffects(x)
}

// CHECK-FR-MAIN-LABEL: sil @$s6Module19noInlineWithEffectsySiAA1XCF
// CHECK-FR-MAIN-NEXT:  [%0!: noescape **]
// CHECK-FR-MAIN-NEXT:  {{^[^[]}}

// CHECK-MAIN-LABEL: sil [noinline] @$s4Main7callit4SiyF :
// CHECK-FR-MAIN:       alloc_ref [stack] $X
// CHECK-LE-MAIN:       = function_ref @$s6Module1XCACycfc
// CHECK-MAIN:       } // end sil function '$s4Main7callit4SiyF'
@inline(never)
public func callit4() -> Int {
  let x = X()
  return inlineWithEffects(x)
}

// CHECK-FR-MAIN-LABEL: sil public_external [noinline] @$s6Module17inlineWithEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MAIN-NEXT:  [%0!: noescape **]
// CHECK-FR-MAIN-NEXT:  {{^[^[]}}
// CHECK-LE-MAIN-LABEL: sil public_external [noinline] @$s6Module17inlineWithEffectsySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-LE-MAIN-NEXT:  [%0!: noescape **]
// CHECK-LE-MAIN-NEXT:  {{^[^[]}}

// CHECK-MAIN-LABEL: sil [noinline] @$s4Main7callit5SiyF :
// CHECK-FR-MAIN:       alloc_ref [stack] $X
// CHECK-LE-MAIN:       alloc_ref $X
// CHECK-MAIN:       } // end sil function '$s4Main7callit5SiyF'
@inline(never)
public func callit5() -> Int {
  let x = X()
  return simpleInline(x)
}

// CHECK-FR-MAIN-LABEL: sil public_external [noinline] @$s6Module12simpleInlineySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MAIN-NEXT:  [%0: noescape **]
// CHECK-FR-MAIN-NEXT:  {{^[^[]}}
// CHECK-LE-MAIN-LABEL: sil public_external [noinline] @$s6Module12simpleInlineySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-LE-MAIN-NEXT:  {{^[^[]}}

// CHECK-FR-MAIN-LABEL: sil @$s6Module14internalCalleeySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int {
// CHECK-FR-MAIN-NEXT:  [%0: noescape **]
// CHECK-FR-MAIN-NEXT:  {{^[^[]}}
// CHECK-LE-MAIN-LABEL: sil @$s6Module14internalCalleeySiAA1XCF : $@convention(thin) (@guaranteed X) -> Int{{$}}

#endif

