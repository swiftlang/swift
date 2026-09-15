// RUN: %target-swift-ide-test -print-module -module-to-print=InheritFRTSubclassing -I %S%{fs-sep}Inputs -source-filename=x -cxx-interoperability-mode=default -print-access -enable-experimental-feature ForeignReferenceTypeSubclassing | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=InheritFRTSubclassing -I %S%{fs-sep}Inputs -source-filename=x -cxx-interoperability-mode=default -print-access | %FileCheck %s --check-prefix=CHECK-OFF

// REQUIRES: swift_feature_ForeignReferenceTypeSubclassing

// CHECK: open class SubclassableShared {

// CHECK: open class DerivedSubclassableShared : SubclassableShared {

// CHECK: public class NonVirtualShared {

// CHECK: public class FinalShared {

// CHECK-OFF: public class SubclassableShared {
// CHECK-OFF: public class DerivedSubclassableShared : SubclassableShared {
// CHECK-OFF: public class NonVirtualShared {
// CHECK-OFF: public class FinalShared {
// CHECK-OFF-NOT: open class
