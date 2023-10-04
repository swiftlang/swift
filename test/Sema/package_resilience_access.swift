// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module %t/Utils.swift \
// RUN:   -module-name Utils -swift-version 5 -I %t \
// RUN:   -package-name mypkg \
// RUN:   -enable-library-evolution \
// RUN:   -emit-module -emit-module-path %t/Utils.swiftmodule

// RUN: %target-swift-frontend -typecheck %t/Client.swift -I %t -swift-version 5 -package-name mypkg -verify

// RUN: %target-swift-frontend -emit-sil %t/Client.swift -package-name mypkg -I %t > %t/Client.sil
// RUN: %FileCheck %s < %t/Client.sil


//--- Utils.swift

// Resilient; public. Acessed indirectly.
public struct PublicStruct {
  public var data: Int
}

// Non-resilient; non-public. Accessed directly.
package struct PkgStruct {
  package var data: Int
}

// Non-resilient but accessed indirectly since generic.
package struct PkgStructGeneric<T> {
  package var data: T
}

// Non-resilient but accessed indirectly; member is of a resilient type.
package struct PkgStructWithPublicMember {
  package var member: PublicStruct
}

// Non-resilient but accessed indirectly; contains existential.
package struct PkgStructWithPublicExistential {
  package var member: any PublicProto
}

// Non-resilient but accessed indirectly; contains existential.
package struct PkgStructWithPkgExistential {
  package var member: any PkgProto
}

// Resilient; public. Acessed indirectly.
public protocol PublicProto {
  var data: Int { get set }
}

// Non-resilient but acessed indirectly; existential.
package protocol PkgProto {
  var data: Int { get set }
}


//--- Client.swift
import Utils

package func f(_ arg: PublicStruct) -> Int {
  return arg.data
}

// CHECK: // f(_:)
// CHECK-NEXT: sil @$s6Client1fySi5Utils12PublicStructVF : $@convention(thin) (@in_guaranteed PublicStruct) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %3, %1
// CHECK-NEXT: bb0(%0 : $*PublicStruct):
// CHECK-NEXT:   debug_value %0 : $*PublicStruct, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = alloc_stack $PublicStruct                  // users: %7, %6, %5, %3
// CHECK-NEXT:   copy_addr %0 to [init] %2 : $*PublicStruct      // id: %3
// CHECK-NEXT:   // function_ref PublicStruct.data.getter
// CHECK-NEXT:   %4 = function_ref @$s5Utils12PublicStructV4dataSivg : $@convention(method) (@in_guaranteed PublicStruct) -> Int // user: %5
// CHECK-NEXT:   %5 = apply %4(%2) : $@convention(method) (@in_guaranteed PublicStruct) -> Int // user: %8
// CHECK-NEXT:   destroy_addr %2 : $*PublicStruct                // id: %6
// CHECK-NEXT:   dealloc_stack %2 : $*PublicStruct               // id: %7
// CHECK-NEXT:   return %5 : $Int                                // id: %8
// CHECK-NEXT: } // end sil function '$s6Client1fySi5Utils12PublicStructVF'


package func g(_ arg: PkgStruct) -> Int {
  return arg.data
}

// CHECK: // g(_:)
// CHECK-NEXT: sil @$s6Client1gySi5Utils9PkgStructVF : $@convention(thin) (PkgStruct) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %2, %1
// CHECK-NEXT: bb0(%0 : $PkgStruct):
// CHECK-NEXT:   debug_value %0 : $PkgStruct, let, name "arg", argno 1 // id: %1
// CHECK-NEXT:   %2 = struct_extract %0 : $PkgStruct, #PkgStruct.data // user: %3
// CHECK-NEXT:   return %2 : $Int                                // id: %3
// CHECK-NEXT: } // end sil function '$s6Client1gySi5Utils9PkgStructVF'

package func m<T>(_ arg: PkgStructGeneric<T>) -> T {
  return arg.data
}

// CHECK: // m<A>(_:)
// CHECK-NEXT: sil @$s6Client1myx5Utils16PkgStructGenericVyxGlF : $@convention(thin) <T> (@in_guaranteed PkgStructGeneric<T>) -> @out T {
// CHECK-NEXT: // %0 "$return_value"                             // user: %4
// CHECK-NEXT: // %1 "arg"                                       // users: %3, %2
// CHECK-NEXT: bb0(%0 : $*T, %1 : $*PkgStructGeneric<T>):
// CHECK-NEXT:   debug_value %1 : $*PkgStructGeneric<T>, let, name "arg", argno 1, expr op_deref // id: %2
// CHECK-NEXT:   %3 = struct_element_addr %1 : $*PkgStructGeneric<T>, #PkgStructGeneric.data // user: %4
// CHECK-NEXT:   copy_addr %3 to [init] %0 : $*T                 // id: %4
// CHECK-NEXT:   %5 = tuple ()                                   // user: %6
// CHECK-NEXT:   return %5 : $()                                 // id: %6
// CHECK-NEXT: } // end sil function '$s6Client1myx5Utils16PkgStructGenericVyxGlF'

package func n(_ arg: PkgStructWithPublicMember) -> Int {
  return arg.member.data
}

// CHECK: // n(_:)
// CHECK-NEXT: sil @$s6Client1nySi5Utils25PkgStructWithPublicMemberVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicMember) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %2, %1
// CHECK-NEXT: bb0(%0 : $*PkgStructWithPublicMember):
// CHECK-NEXT:   debug_value %0 : $*PkgStructWithPublicMember, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = struct_element_addr %0 : $*PkgStructWithPublicMember, #PkgStructWithPublicMember.member // user: %4
// CHECK-NEXT:   %3 = alloc_stack $PublicStruct                  // users: %12, %10, %6, %4
// CHECK-NEXT:   copy_addr %2 to [init] %3 : $*PublicStruct      // id: %4
// CHECK-NEXT:   %5 = alloc_stack $PublicStruct                  // users: %11, %9, %8, %6
// CHECK-NEXT:   copy_addr %3 to [init] %5 : $*PublicStruct      // id: %6
// CHECK-NEXT:   // function_ref PublicStruct.data.getter
// CHECK-NEXT:   %7 = function_ref @$s5Utils12PublicStructV4dataSivg : $@convention(method) (@in_guaranteed PublicStruct) -> Int // user: %8
// CHECK-NEXT:   %8 = apply %7(%5) : $@convention(method) (@in_guaranteed PublicStruct) -> Int // user: %13
// CHECK-NEXT:   destroy_addr %5 : $*PublicStruct                // id: %9
// CHECK-NEXT:   destroy_addr %3 : $*PublicStruct                // id: %10
// CHECK-NEXT:   dealloc_stack %5 : $*PublicStruct               // id: %11
// CHECK-NEXT:   dealloc_stack %3 : $*PublicStruct               // id: %12
// CHECK-NEXT:   return %8 : $Int                                // id: %13
// CHECK-NEXT: } // end sil function '$s6Client1nySi5Utils25PkgStructWithPublicMemberVF'

package func p(_ arg: PkgStructWithPublicExistential) -> any PublicProto {
  return arg.member
}

// CHECK: // p(_:)
// CHECK-NEXT: sil @$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPublicExistential) -> @out any PublicProto {
// CHECK-NEXT: // %0 "$return_value"                             // user: %4
// CHECK-NEXT: // %1 "arg"                                       // users: %3, %2
// CHECK-NEXT: bb0(%0 : $*any PublicProto, %1 : $*PkgStructWithPublicExistential):
// CHECK-NEXT:   debug_value %1 : $*PkgStructWithPublicExistential, let, name "arg", argno 1, expr op_deref // id: %2
// CHECK-NEXT:   %3 = struct_element_addr %1 : $*PkgStructWithPublicExistential, #PkgStructWithPublicExistential.member // user: %4
// CHECK-NEXT:   copy_addr %3 to [init] %0 : $*any PublicProto   // id: %4
// CHECK-NEXT:   %5 = tuple ()                                   // user: %6
// CHECK-NEXT:   return %5 : $()                                 // id: %6
// CHECK-NEXT: } // end sil function '$s6Client1py5Utils11PublicProto_pAC013PkgStructWithC11ExistentialVF'

package func q(_ arg: PkgStructWithPkgExistential) -> any PkgProto {
  return arg.member
}

// CHECK: // q(_:)
// CHECK-NEXT: sil @$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF : $@convention(thin) (@in_guaranteed PkgStructWithPkgExistential) -> @out any PkgProto {
// CHECK-NEXT: // %0 "$return_value"                             // user: %4
// CHECK-NEXT: // %1 "arg"                                       // users: %3, %2
// CHECK-NEXT: bb0(%0 : $*any PkgProto, %1 : $*PkgStructWithPkgExistential):
// CHECK-NEXT:   debug_value %1 : $*PkgStructWithPkgExistential, let, name "arg", argno 1, expr op_deref // id: %2
// CHECK-NEXT:   %3 = struct_element_addr %1 : $*PkgStructWithPkgExistential, #PkgStructWithPkgExistential.member // user: %4
// CHECK-NEXT:   copy_addr %3 to [init] %0 : $*any PkgProto      // id: %4
// CHECK-NEXT:   %5 = tuple ()                                   // user: %6
// CHECK-NEXT:   return %5 : $()                                 // id: %6
// CHECK-NEXT: } // end sil function '$s6Client1qy5Utils8PkgProto_pAC0c10StructWithC11ExistentialVF'

package func r(_ arg: PublicProto) -> Int {
  return arg.data
}

// CHECK: // r(_:)
// CHECK-NEXT: sil @$s6Client1rySi5Utils11PublicProto_pF : $@convention(thin) (@in_guaranteed any PublicProto) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %2, %1
// CHECK-NEXT: bb0(%0 : $*any PublicProto):
// CHECK-NEXT:   debug_value %0 : $*any PublicProto, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = open_existential_addr immutable_access %0 : $*any PublicProto
// CHECK-NEXT:   %3 = alloc_stack $@opened
// CHECK-NEXT:   copy_addr %2 to [init] %3 : $*@opened
// CHECK-NEXT:   %5 = witness_method $@opened
// CHECK-NEXT:   %6 = apply %5<@opened
// CHECK-NEXT:   destroy_addr %3 : $*@opened
// CHECK-NEXT:   dealloc_stack %3 : $*@opened
// CHECK-NEXT:   return %6 : $Int
// CHECK-NEXT: } // end sil function '$s6Client1rySi5Utils11PublicProto_pF'

package func s(_ arg: PkgProto) -> Int {
  return arg.data
}
// CHECK: // s(_:)
// CHECK-NEXT: sil @$s6Client1sySi5Utils8PkgProto_pF : $@convention(thin) (@in_guaranteed any PkgProto) -> Int {
// CHECK-NEXT: // %0 "arg"                                       // users: %2, %1
// CHECK-NEXT: bb0(%0 : $*any PkgProto):
// CHECK-NEXT:   debug_value %0 : $*any PkgProto, let, name "arg", argno 1, expr op_deref // id: %1
// CHECK-NEXT:   %2 = open_existential_addr immutable_access %0 : $*any PkgProto to $*@opened
// CHECK-NEXT:   %3 = alloc_stack $@opened
// CHECK-NEXT:   copy_addr %2 to [init] %3 : $*@opened
// CHECK-NEXT:   %5 = witness_method $@opened
// CHECK-NEXT:   %6 = apply %5<@opened
// CHECK-NEXT:   destroy_addr %3 : $*@opened
// CHECK-NEXT:   dealloc_stack %3 : $*@opened
// CHECK-NEXT:   return %6 : $Int
// CHECK-NEXT: } // end sil function '$s6Client1sySi5Utils8PkgProto_pF'

