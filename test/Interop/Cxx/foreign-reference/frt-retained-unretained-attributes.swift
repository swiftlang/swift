// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -disable-availability-checking -diagnostic-style llvm %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import FunctionsAndMethodsReturningFRT

func testFriendFunctionsReturningFRT() {
    let frtLocalVar1 = returnInstanceOfFRTStruct()
    // CHECK: function_ref @{{.*}}returnInstanceOfFRTStruct{{.*}} : $@convention(c) () -> Optional<FRTStruct>

    let frtLocalVar2 = returnInstanceOfFRTStructWithAttrReturnsRetained()
    // CHECK: function_ref @{{.*}}returnInstanceOfFRTStructWithAttrReturnsRetained{{.*}} : $@convention(c) () -> @owned Optional<FRTStruct>

    let frtLocalVar3 = returnInstanceOfFRTStructWithAttrReturnsUnretained()
    // CHECK: function_ref @{{.*}}returnInstanceOfFRTStructWithAttrReturnsUnretained{{.*}} : $@convention(c) () -> Optional<FRTStruct>
}

func testFreeFunctionsWithoutAttrubutes() {
    let frtLocalVar1 = global_function_returning_FRT()
    // CHECK: function_ref @{{.*}}global_function_returning_FRT{{.*}} : $@convention(c) () -> FRTStruct

    // Free/global functions having copy/create in the function name are passed as owned by default
    let frtLocalVar2 = global_function_returning_copy()
    // CHECK: function_ref @{{.*}}global_function_returning_copy{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = global_function_returning_create()
    // CHECK: function_ref @{{.*}}global_function_returning_create{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar4 = global_function_returning_init()
    // CHECK: function_ref @{{.*}}global_function_returning_init{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = global_function_returning_clone()
    // CHECK: function_ref @{{.*}}global_function_returning_clone{{.*}} : $@convention(c) () -> FRTStruct
}

func testFreeFunctionsWithAttrubuteReturnsRetained() {
    let frtLocalVar1 = global_function_returning_FRT_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_function_returning_FRT_with_attr_returns_retained{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar2 = global_function_returning_copy_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_function_returning_copy_with_attr_returns_retained{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = global_function_returning_create_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_function_returning_create_with_attr_returns_retained{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar4 = global_function_returning_init_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_function_returning_init_with_attr_returns_retained{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar5 = global_function_returning_clone_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_function_returning_clone_with_attr_returns_retained{{.*}} : $@convention(c) () -> @owned FRTStruct
}

func testFreeFunctionsWithAttrubuteReturnsUnretained() {
    let frtLocalVar1 = global_function_returning_FRT_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_function_returning_FRT_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = global_function_returning_copy_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_function_returning_copy_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar3 = global_function_returning_create_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_function_returning_create_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = global_function_returning_init_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_function_returning_init_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = global_function_returning_clone_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_function_returning_clone_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct
}

func testStaticFreeFunctions() {
    let frtLocalVar1 = global_static_function_returning_FRT()
    // CHECK: function_ref @{{.*}}global_static_function_returning_FRT{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = global_static_function_returning_FRT_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_static_function_returning_FRT_with_attr_returns_retained{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = global_static_function_returning_FRT_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_static_function_returning_FRT_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = global_static_function_returning_copy()
    // CHECK: function_ref @{{.*}}global_static_function_returning_copy{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar5 = global_static_function_returning_create()
    // CHECK: function_ref @{{.*}}global_static_function_returning_create{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar6 = global_static_function_returning_copy_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_static_function_returning_copy_with_attr_returns_retained{{.*}} : $@convention(c) () ->  @owned FRTStruct

    let frtLocalVar7 = global_static_function_returning_copy_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_static_function_returning_copy_with_attr_returns_unretained{{.*}} : $@convention(c) () -> FRTStruct
}

//  Testing Global/free C++ functions without _Nonnull
func testtFreeFunctionsWithoutNonnull() {
    let frtLocalVar1 = global_function_returning_FRT_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_FRT_wihout_Nonnull{{.*}} : $@convention(c) () -> Optional<FRTStruct>

    let frtLocalVar2 = global_function_returning_FRT_with_attr_returns_retained_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_FRT_with_attr_returns_retained_wihout_Nonnull{{.*}} : $@convention(c) () ->  @owned Optional<FRTStruct>
   
    let frtLocalVar3 = global_function_returning_FRT_with_attr_returns_unretained_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_FRT_with_attr_returns_unretained_wihout_Nonnull{{.*}} : $@convention(c) () -> Optional<FRTStruct>

    let frtLocalVar4 = global_function_returning_copy_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_copy_wihout_Nonnull{{.*}} : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar5 = global_function_returning_create_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_create_wihout_Nonnull{{.*}} : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar6 = global_function_returning_copy_with_attr_returns_retained_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_copy_with_attr_returns_retained_wihout_Nonnull{{.*}} : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar7 = global_function_returning_copy_with_attr_returns_unretained_wihout_Nonnull()
    // CHECK: function_ref @{{.*}}global_function_returning_copy_with_attr_returns_unretained_wihout_Nonnull{{.*}} : $@convention(c) () -> Optional<FRTStruct>
}

func testStaticMethodsWithoutAttrubutes() {
    let frtLocalVar1 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_copy()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_copy{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar3 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_create()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_create{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_init()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_init{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_clone()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_clone{{.*}} : $@convention(c) () -> FRTStruct
}

func testStaticMethodsWithAttrubuteReturnsRetained() {
    let frtLocalVar1 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_copy()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_copy{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_create()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_create{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar4 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_init()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_init{{.*}} : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar5 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_clone()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_clone{{.*}} : $@convention(c) () -> @owned FRTStruct
}

func testStaticMethodsWithAttrubuteReturnsUnretained() {
    let frtLocalVar1 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_copy()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_copy{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar3 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_create()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_create{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_init()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_init{{.*}} : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_clone()
    // CHECK: function_ref @{{.*}}StaticMethodReturningFRT_clone{{.*}} : $@convention(c) () -> FRTStruct
}

func testFreeFunctionsReturningNonFRT() {
    let frtLocalVar1 = global_function_returning_non_FRT()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar4 = global_function_returning_non_FRT_create()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT_create{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar5 = global_function_returning_non_FRT_copy()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT_copy{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 
}

func testStaticMethodsReturningNonFRT() {
    let frtLocalVar1 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRT{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar4 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT_create()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRT_create{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar5 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT_copy()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRT_copy{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 
}

func testtFreeFunctionsTemplated(frt : FRTStruct, nonFrt: NonFRTStruct) {
    let frtLocalVar1 : Int = 1;
    
    let frtLocalVar2 = global_templated_function_returning_FRT(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT{{.*}} : $@convention(c) (Int) -> FRTStruct
    
    let frtLocalVar3 = global_templated_function_returning_FRT_copy(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_copy{{.*}} : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar4 = global_templated_function_returning_FRT_create(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_create{{.*}} : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar5 = global_templated_function_returning_FRT_init(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_init{{.*}} : $@convention(c) (Int) -> FRTStruct
    
    let frtLocalVar6 = global_templated_function_returning_FRT_clone(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_clone{{.*}} : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar7 = global_templated_function_returning_FRT_with_attr_returns_retained(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_with_attr_returns_retained{{.*}} : $@convention(c) (Int) -> @owned FRTStruct
    
    let frtLocalVar8 = global_templated_function_returning_FRT_copy_with_attr_returns_retained(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_copy_with_attr_returns_retained{{.*}} : $@convention(c) (Int) -> @owned FRTStruct
    
    let frtLocalVar9 = global_templated_function_returning_FRT_create_with_attr_returns_retained(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_create_with_attr_returns_retained{{.*}} : $@convention(c) (Int) -> @owned FRTStruct
    
    let frtLocalVar10 = global_templated_function_returning_FRT_with_attr_returns_unretained(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_with_attr_returns_unretained{{.*}} : $@convention(c) (Int) -> FRTStruct
    
    let frtLocalVar11 = global_templated_function_returning_FRT_copy_with_attr_returns_unretained(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_copy_with_attr_returns_unretained{{.*}} : $@convention(c) (Int) -> FRTStruct
    
    let frtLocalVar12 = global_templated_function_returning_FRT_create_with_attr_returns_unretained(frtLocalVar1)
    // CHECK: function_ref @{{.*}}global_templated_function_returning_FRT_create_with_attr_returns_unretained{{.*}} : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar13 = global_function_returning_templated_retrun_frt(frt)
    // CHECK: function_ref @{{.*}}global_function_returning_templated_retrun_frt{{.*}} : $@convention(c) (FRTStruct) -> FRTStruct

    let frtLocalVar14 = global_function_returning_templated_retrun_frt_owned(frt)
    // CHECK: function_ref @{{.*}}global_function_returning_templated_retrun_frt_owned{{.*}} : $@convention(c) (FRTStruct) -> @owned FRTStruct

    let nonFrtLocalVar1 = global_function_returning_templated_retrun_frt_owned(nonFrt)
    // CHECK: function_ref @{{.*}}global_function_returning_templated_retrun_frt_owned{{.*}} : $@convention(c) (NonFRTStruct) -> NonFRTStruct
}

func testVirtualMethods(base: Base, derived: Derived) {
    var mutableBase = base
    var mutableDerived = derived

    var frt1 = mutableBase.VirtualMethodReturningFRTUnowned()
    // CHECK: function_ref @{{.*}}VirtualMethodReturningFRTUnowned{{.*}} : $@convention(cxx_method) (@inout Base) -> FRTStruct
    
    var frt2 = mutableDerived.VirtualMethodReturningFRTUnowned()
    // CHECK: function_ref @{{.*}}VirtualMethodReturningFRTUnowned{{.*}} : $@convention(cxx_method) (@inout Derived) -> FRTStruct
    
    var frt3 = mutableBase.VirtualMethodReturningFRTOwned()
    // CHECK: function_ref @{{.*}}VirtualMethodReturningFRTOwned{{.*}} : $@convention(cxx_method) (@inout Base) ->  @owned FRTStruct
    
    var frt4 = mutableDerived.VirtualMethodReturningFRTOwned()
    // CHECK: function_ref @{{.*}}VirtualMethodReturningFRTOwned{{.*}} : $@convention(cxx_method) (@inout Derived) -> @owned FRTStruct
}

func testDefaultOwnershipAnnotation() {
  let _ = DefaultOwnershipConventionOnCXXForeignRefType.returnRefTyDefUnretained()
  // CHECK: function_ref {{.*}}returnRefTyDefUnretained{{.*}} : $@convention(c) () -> DefaultOwnershipConventionOnCXXForeignRefType.RefTyDefUnretained

  let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretained()
  // CHECK: function_ref {{.*}}returnRefTyDefUnretained{{.*}} : $@convention(c) () -> FunctionAnnotationHasPrecedence.RefTyDefUnretained

  let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretainedAnnotatedRetained()
  // CHECK: function_ref {{.*}}returnRefTyDefUnretainedAnnotatedRetained{{.*}} : $@convention(c) () -> @owned FunctionAnnotationHasPrecedence.RefTyDefUnretained

  let _ = DefaultOwnershipInheritance.createBaseType()
  // CHECK: function_ref {{.*}}createBaseType{{.*}} : $@convention(c) () -> DefaultOwnershipInheritance.BaseType

  let _ = DefaultOwnershipInheritance.createDerivedType()
  // CHECK: function_ref {{.*}}createDerivedType{{.*}} : $@convention(c) () -> DefaultOwnershipInheritance.DerivedType

  let _ = DefaultOwnershipInheritance.createDerivedType2()
  // CHECK: function_ref {{.*}}createDerivedType2{{.*}} : $@convention(c) () -> DefaultOwnershipInheritance.DerivedType2

  let _ = DefaultOwnershipInheritance.createBaseTypeNonDefault()
  // CHECK: function_ref {{.*}}createBaseTypeNonDefault{{.*}} : $@convention(c) () -> @owned DefaultOwnershipInheritance.BaseTypeNonDefault

  let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefault()
  // CHECK: function_ref {{.*}}createDerivedTypeNonDefault{{.*}} : $@convention(c) () -> @owned DefaultOwnershipInheritance.DerivedTypeNonDefault

  let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefaultUnretained()
  // CHECK: function_ref {{.*}}createDerivedTypeNonDefaultUnretained{{.*}} : $@convention(c) () -> DefaultOwnershipInheritance.DerivedTypeNonDefault
}
