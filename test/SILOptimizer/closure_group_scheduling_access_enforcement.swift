// RUN: %target-swift-frontend -emit-sil %s -module-name E -sil-verify-all -o - | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-sil %s -module-name E -Xllvm -sil-print-closure-groups -o /dev/null 2>&1 | %FileCheck %s --check-prefix=GROUPS
// RUN: %target-swift-frontend -emit-sil %s -module-name E -Xllvm -sil-print-after=access-enforcement-selection -Xllvm '-sil-print-function=$s1E7escapesSiyFyyXEfU0_' -o /dev/null 2>&1 | %FileCheck %s --check-prefix=PRINT

// AccessEnforcementSelection runs once per closure group. A closure must be in
// its parent's group to see the parent's dynamic captures.

@usableFromInline func modify(_ body: () -> ()) { body() }

var saved: (() -> Int)?

// x does not escape: the closure's access is static.
// GROUPS: Closure group for access-enforcement-selection: $s1E5localSiyF $s1E5localSiyFyyXEfU_{{$}}
// SIL-LABEL: sil private @$s1E5localSiyFyyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// SIL: begin_access [modify] [static]
public func local() -> Int {
  var x = 0
  modify { x += 1 }
  return x
}

// x escapes, so the non-escaping closure's access is dynamic. The escaping
// closure captures the box, not the address, and joins the group too.
// GROUPS: Closure group for access-enforcement-selection: $s1E7escapesSiyF $s1E7escapesSiyFSiycfU_ $s1E7escapesSiyFyyXEfU0_{{$}}
// SIL-LABEL: sil private @$s1E7escapesSiyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// SIL: begin_access [modify] [dynamic]
public func escapes() -> Int {
  var x = 0
  saved = { x }
  modify { x += 1 }
  return x
}

// The dynamic capture propagates through a middle closure to the inner one.
// GROUPS: Closure group for access-enforcement-selection: $s1E6nestedSiyF $s1E6nestedSiyFSiycfU_ $s1E6nestedSiyFyyXEfU0_ $s1E6nestedSiyFyyXEfU0_yyXEfU_{{$}}
// SIL-LABEL: sil private @$s1E6nestedSiyFyyXEfU0_yyXEfU_ : $@convention(thin) (@inout_aliasable Int) -> () {
// SIL: begin_access [modify] [dynamic]
public func nested() -> Int {
  var x = 0
  saved = { x }
  modify {
    modify { x += 1 }
  }
  return x
}

// -sil-print-after prints a member after the pass ran on its group.
// PRINT: sil private [ossa] @$s1E7escapesSiyFyyXEfU0_ : $@convention(thin) (@inout_aliasable Int) -> () {
// PRINT: begin_access [modify] [dynamic]

// closure_group_scheduling_back_deployed.swift covers @backDeployed, on macOS.
