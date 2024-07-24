// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -diagnostic-style llvm %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

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

    let frtLocalVar2 = global_function_returning_non_FRT_with_attr_returns_retained()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT_with_attr_returns_retained{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar3 = global_function_returning_non_FRT_with_attr_returns_unretained()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT_with_attr_returns_unretained{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar4 = global_function_returning_non_FRT_create()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT_create{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar5 = global_function_returning_non_FRT_copy()
    // CHECK: function_ref @{{.*}}global_function_returning_non_FRT_copy{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 
}

func testStaticMethodsReturningNonFRT() {
    let frtLocalVar1 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRT{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar2 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRTWithAttrReturnsRetained()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRTWithAttrReturnsRetained{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar3 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRTWithAttrReturnsUnretained()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRTWithAttrReturnsUnretained{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar4 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT_create()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRT_create{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 

    let frtLocalVar5 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT_copy()
    // CHECK: function_ref @{{.*}}StaticMethodReturningNonFRT_copy{{.*}} : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct> 
}

func testtFreeFunctionsTemplated() {
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

}
