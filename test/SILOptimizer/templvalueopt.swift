// RUN: %target-swift-frontend -module-name=test -O -emit-sil  %s | %FileCheck %s

// RUN: %empty-directory(%t) 
// RUN: %target-build-swift -O -module-name=test %s -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s -check-prefix=CHECK-OUTPUT

// REQUIRES: executable_test
// REQUIRES: CPU=arm64 || CPU=x86_64

// Check that the compiler creates optimal code for address-only enum initializations.

public enum Either<Left, Right> {
  case left(Left), right(Right)
}

internal struct AddressOnlyPayload {
  let a: Any
  let i: Int
}

public struct TestStruct {
  internal var e: Either<AddressOnlyPayload, Int>

  // CHECK-LABEL: sil [noinline] @$s4test10TestStructVyACyp_SitcfC
  // CHECK: [[E:%[0-9]+]] = struct_element_addr %0 : $*TestStruct, #TestStruct.e
  // CHECK: [[LEFT:%[0-9]+]] = init_enum_data_addr [[E]] : $*Either<AddressOnlyPayload, Int>, #Either.left!enumelt
  // CHECK: [[A:%[0-9]+]] = struct_element_addr [[LEFT]] : $*AddressOnlyPayload, #AddressOnlyPayload.a
  // CHECK: copy_addr [take] %1 to [initialization] [[A]] : $*Any
  // CHECK: [[I:%[0-9]+]] = struct_element_addr [[LEFT]] : $*AddressOnlyPayload, #AddressOnlyPayload.i
  // CHECK: store %2 to [[I]] : $*Int
  // CHECK: inject_enum_addr [[E]] : $*Either<AddressOnlyPayload, Int>, #Either.left!enumelt
  // CHECK: } // end sil function '$s4test10TestStructVyACyp_SitcfC'
  @inline(never)
  public init(_ a: Any, _ i: Int) {
    e = Either.left(AddressOnlyPayload(a: a, i: i))
  }
}

// CHECK-LABEL: sil [noinline] @$s4test13createAnyLeftyAA6EitherOyypSgSiGypF
// CHECK:  [[E:%[0-9]+]] = init_enum_data_addr %0 : $*Either<Optional<Any>, Int>, #Either.left!enumelt
// CHECK:  [[SOME:%[0-9]+]] = init_enum_data_addr [[E]] : $*Optional<Any>, #Optional.some!enumelt
// CHECK:  copy_addr %1 to [initialization] [[SOME]] : $*Any
// CHECK:  inject_enum_addr [[E]] : $*Optional<Any>, #Optional.some!enumelt
// CHECK:  inject_enum_addr %0 : $*Either<Optional<Any>, Int>, #Either.left!enumelt
// CHECK: } // end sil function '$s4test13createAnyLeftyAA6EitherOyypSgSiGypF'
@inline(never)
public func createAnyLeft(_ a: Any) -> Either<Any?, Int> {
  return Either.left(a)
}


// CHECK-OUTPUT: TestStruct(e: test.Either<test.AddressOnlyPayload, Swift.Int>.left(test.AddressOnlyPayload(a: 27, i: 1)))
// CHECK-OUTPUT: TestStruct(e: test.Either<test.AddressOnlyPayload, Swift.Int>.left(test.AddressOnlyPayload(a: "A non-trivial value", i: 1)))
print(TestStruct(27, 1))
print(TestStruct("A non-trivial value", 1))

// CHECK-OUTPUT: left(Optional(27))
// CHECK-OUTPUT: left(Optional("A non-trivial value"))
print(createAnyLeft(27))
print(createAnyLeft("A non-trivial value"))

