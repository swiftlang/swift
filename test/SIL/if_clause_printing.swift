// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

// Check that we don't crash when trying to print an inactive if-clause.

// CHECK:      struct TestStruct
// CHECK-NEXT:   internal enum TheRealOne
// CHECK-NEXT:     case B

struct TestStruct {
#if abc
  internal enum Unused {
    case A(AnyObject)
  }
#else
  internal enum TheRealOne {
    case B(AnyObject)
  }
#endif
}

