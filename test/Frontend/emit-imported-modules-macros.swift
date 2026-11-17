// Verify that -emit-imported-modules does not attempt to resolve or expand
// freestanding or attached macros during parse-only module import collection.

// RUN: %target-swift-frontend -emit-imported-modules -parse-as-library -module-name Test -D ENABLED %s 2>&1 | %FileCheck %s --match-full-lines --implicit-check-not=ConditionalDisabled --implicit-check-not=error: --implicit-check-not=warning:
// RUN: %target-swift-frontend -emit-imported-modules -module-name Test -D ENABLED %s 2>&1 | %FileCheck %s --match-full-lines --implicit-check-not=ConditionalDisabled --implicit-check-not=error: --implicit-check-not=warning:

// CHECK:      Foo
// CHECK-NEXT: A
// CHECK-NEXT: Bar
// CHECK-NEXT: ConditionalEnabled

import Foo
import A.B.C
import enum Bar.Member

#if ENABLED
import ConditionalEnabled
#else
import ConditionalDisabled
#endif

#if DISABLED
import ConditionalDisabled
#endif

// Top-level freestanding declaration macros.
#topLevelMacro()
#topLevelMacroWithArgs(1, "hello")

// Attached macros on top-level declarations, nominal types, and members.
@attachedPeer
func topLevelFunc() {}

@attachedMember
@attachedExtension(arg: 42)
struct S {
  #memberMacro()

  @attachedAccessor
  var property: Int = 0

  @attachedPeer
  struct Nested {
    #nestedMemberMacro()
  }
}

@attachedMember
class C {
  #classMemberMacro()
}

@attachedMember
enum E {
  #enumMemberMacro()
}

@attachedMember
actor A {
  #actorMemberMacro()
}

@attachedMember
protocol P {
  #protocolMemberMacro()
}

@attachedMemberAttribute
extension S {
  #extensionMemberMacro()
}
