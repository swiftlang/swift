// RUN: %target-swift-ide-test -print-module -module-to-print=CountedByClang -plugin-path %swift-plugin-dir -I %S/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Werror -Xcc -Wno-nullability-completeness -Xcc -Wno-div-by-zero -Xcc -Wno-pointer-to-int-cast  | %FileCheck %s

// REQUIRES: swift_feature_SafeInteropWrappers

// These functions use __counted_by annotations that are not syntactically valid
// in Swift, so they should not be Swiftified

// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} derefLen
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} lNot
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} lAnd
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} lOr
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} floatCastToInt
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} pointerCastToInt
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} nanAsInt
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} unsignedLiteral
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} longLiteral
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} sizeofType
// CHECK-NOT: @_alwaysEmitIntoClient {{.*}} sizeofParam
