// RUN: %target-swift-ide-test -print-module -module-to-print=NonMemberInline -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK:      func + (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func - (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func * (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func / (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func % (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func ^ (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func & (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func | (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func << (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func >> (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> LoadableIntWrapper
// CHECK-NEXT: func < (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool
// CHECK-NEXT: func > (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool
// CHECK-NEXT: func == (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool
// CHECK-NEXT: func != (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool
// CHECK-NEXT: func <= (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool
// CHECK-NEXT: func >= (lhs: LoadableIntWrapper, rhs: LoadableIntWrapper) -> Bool

// CHECK:      func && (lhs: LoadableBoolWrapper, rhs: LoadableBoolWrapper) -> LoadableBoolWrapper
// CHECK-NEXT: func || (lhs: LoadableBoolWrapper, rhs: LoadableBoolWrapper) -> LoadableBoolWrapper
