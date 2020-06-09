// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-sil -enable-objc-interop -o - -emit-module-path %t/Lib.swiftmodule -module-name Lib -I %S/Inputs/custom-modules %s > /dev/null

// RUN: %target-swift-ide-test -enable-objc-interop -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck -check-prefix CHECK -check-prefix CHECK-ALWAYS %s

// RUN: %target-swift-ide-test -enable-objc-interop -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD | %FileCheck -check-prefix CHECK-RECOVERY -check-prefix CHECK-ALWAYS %s

// RUN: %target-swift-frontend -typecheck -enable-objc-interop -I %t -I %S/Inputs/custom-modules -Xcc -DBAD -DTEST %s -verify

#if TEST

import Lib

func use(_: C2_CRTPish) {}
func use(_: C3_NestingCRTPish) {}
func use(_: C4_ExtensionNestingCRTPish) {}

// FIXME: Better to import the class and make it unavailable.
func use(_: A1_Sub) {} // expected-error {{cannot find type 'A1_Sub' in scope}}
func use(_: A2_Grandchild) {} // expected-error {{cannot find type 'A2_Grandchild' in scope}}

#else // TEST

import SuperclassObjC

// CHECK-LABEL: class A1_Sub : DisappearingSuperclass {
// CHECK-RECOVERY-NOT: class A1_Sub
public class A1_Sub: DisappearingSuperclass {}
// CHECK-LABEL: class A2_Grandchild : A1_Sub {
// CHECK-RECOVERY-NOT: class A2_Grandchild
public class A2_Grandchild: A1_Sub {}

// CHECK-LABEL: class B1_ConstrainedGeneric<T> where T : DisappearingSuperclass {
// CHECK-RECOVERY-NOT: class B1_ConstrainedGeneric
public class B1_ConstrainedGeneric<T> where T: DisappearingSuperclass {}

// CHECK-LABEL: struct B2_ConstrainedGeneric<T> where T : DisappearingSuperclass {
// CHECK-RECOVERY-NOT: struct B2_ConstrainedGeneric
public struct B2_ConstrainedGeneric<T> where T: DisappearingSuperclass {}

// CHECK-ALWAYS-LABEL: class C1_GenericBase<T> {
public class C1_GenericBase<T> {}

// CHECK-ALWAYS-LABEL: class C2_CRTPish : C1_GenericBase<C2_CRTPish> {
public class C2_CRTPish: C1_GenericBase<C2_CRTPish> {}
// CHECK-ALWAYS-LABEL: class C3_NestingCRTPish : C1_GenericBase<C3_NestingCRTPish.Nested> {
public class C3_NestingCRTPish: C1_GenericBase<C3_NestingCRTPish.Nested> {
  public struct Nested {}
}
// CHECK-ALWAYS-LABEL: class C4_ExtensionNestingCRTPish : C1_GenericBase<C4_ExtensionNestingCRTPish.Nested> {
public class C4_ExtensionNestingCRTPish: C1_GenericBase<C4_ExtensionNestingCRTPish.Nested> {}
extension C4_ExtensionNestingCRTPish {
  public struct Nested {}
}

#endif // TEST
