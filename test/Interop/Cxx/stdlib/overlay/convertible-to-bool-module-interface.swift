// RUN: %target-swift-ide-test -print-module -module-to-print=ConvertibleToBool -source-filename=x -I %S/Inputs -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: struct BoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct NonConstBoolBox {
// CHECK: }

// CHECK: struct DualOverloadBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct ExplicitBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct BoolBoxWithOtherConversions : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct DeletedBoolBox {
// CHECK: }

// CHECK: struct InheritedBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct OverriddenBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct AnotherBoolBase : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct AmbiguousBoolBox {
// CHECK: }

// CHECK: struct VirtualBoolLeft : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct VirtualBoolRight : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct VirtualDiamondBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct NonVirtualBoolLeft : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct NonVirtualBoolRight : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct DiamondBoolBox {
// CHECK: }

// CHECK: struct PrivateBoolBox {
// CHECK: }

// CHECK: struct ProtectedBoolBox {
// CHECK: }

// CHECK: struct PrivateInheritedBoolBox {
// CHECK: }

// CHECK: struct ProtectedInheritedBoolBox {
// CHECK: }

// CHECK: struct PublicUsingBoolBox : CxxConvertibleToBool {
// CHECK: }

// CHECK: struct ProtectedUsingBoolBox {
// CHECK: }
