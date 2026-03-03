// RUN: %target-swift-emit-sil -I %S/Inputs -cxx-interoperability-mode=upcoming-swift -disable-availability-checking -diagnostic-style llvm %s -validate-tbd-against-ir=none -Xcc -fignore-exceptions | %FileCheck %s

import FunctionsAndMethodsReturningFRT

func testFriendFunctionsReturningFRT() {
    let frtLocalVar1 = returnInstanceOfFRTStruct()
    // CHECK: function_ref @$sSo25returnInstanceOfFRTStructSo0D0VSgyFTo : $@convention(c) () -> Optional<FRTStruct>

    let frtLocalVar2 = returnInstanceOfFRTStructWithAttrReturnsRetained()
    // CHECK: function_ref @$sSo48returnInstanceOfFRTStructWithAttrReturnsRetainedSo0D0VSgyFTo : $@convention(c) () -> @owned Optional<FRTStruct>

    let frtLocalVar3 = returnInstanceOfFRTStructWithAttrReturnsUnretained()
    // CHECK: function_ref @$sSo50returnInstanceOfFRTStructWithAttrReturnsUnretainedSo0D0VSgyFTo : $@convention(c) () -> Optional<FRTStruct>
}

func testFreeFunctionsWithoutAttrubutes() {
    let frtLocalVar1 = global_function_returning_FRT()
    // CHECK: function_ref @$sSo29global_function_returning_FRTSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    // Free/global functions having copy/create in the function name are passed as owned by default
    let frtLocalVar2 = global_function_returning_copy()
    // CHECK: function_ref @$sSo30global_function_returning_copySo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = global_function_returning_create()
    // CHECK: function_ref @$sSo32global_function_returning_createSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar4 = global_function_returning_init()
    // CHECK: function_ref @$sSo30global_function_returning_initSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = global_function_returning_clone()
    // CHECK: function_ref @$sSo31global_function_returning_cloneSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct
}

func testFreeFunctionsWithAttrubuteReturnsRetained() {
    let frtLocalVar1 = global_function_returning_FRT_with_attr_returns_retained()
    // CHECK: function_ref @$sSo56global_function_returning_FRT_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar2 = global_function_returning_copy_with_attr_returns_retained()
    // CHECK: function_ref @$sSo57global_function_returning_copy_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = global_function_returning_create_with_attr_returns_retained()
    // CHECK: function_ref @$sSo59global_function_returning_create_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar4 = global_function_returning_init_with_attr_returns_retained()
    // CHECK: function_ref @$sSo57global_function_returning_init_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar5 = global_function_returning_clone_with_attr_returns_retained()
    // CHECK: function_ref @$sSo58global_function_returning_clone_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct
}

func testFreeFunctionsWithAttrubuteReturnsUnretained() {
    let frtLocalVar1 = global_function_returning_FRT_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo58global_function_returning_FRT_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = global_function_returning_copy_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo59global_function_returning_copy_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar3 = global_function_returning_create_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo61global_function_returning_create_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = global_function_returning_init_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo59global_function_returning_init_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = global_function_returning_clone_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo60global_function_returning_clone_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct
}

func testStaticFreeFunctions() {
    let frtLocalVar1 = global_static_function_returning_FRT()
    // CHECK: function_ref @$sSo36global_static_function_returning_FRTSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = global_static_function_returning_FRT_with_attr_returns_retained()
    // CHECK: function_ref @$sSo63global_static_function_returning_FRT_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = global_static_function_returning_FRT_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo65global_static_function_returning_FRT_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = global_static_function_returning_copy()
    // CHECK: function_ref @$sSo37global_static_function_returning_copySo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar5 = global_static_function_returning_create()
    // CHECK: function_ref @$sSo39global_static_function_returning_createSo9FRTStructVyFTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar6 = global_static_function_returning_copy_with_attr_returns_retained()
    // CHECK: function_ref @$sSo64global_static_function_returning_copy_with_attr_returns_retainedSo9FRTStructVyFTo : $@convention(c) () ->  @owned FRTStruct

    let frtLocalVar7 = global_static_function_returning_copy_with_attr_returns_unretained()
    // CHECK: function_ref @$sSo66global_static_function_returning_copy_with_attr_returns_unretainedSo9FRTStructVyFTo : $@convention(c) () -> FRTStruct
}

//  Testing Global/free C++ functions without _Nonnull
func testtFreeFunctionsWithoutNonnull() {
    let frtLocalVar1 = global_function_returning_FRT_wihout_Nonnull()
    // CHECK: function_ref @$sSo44global_function_returning_FRT_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () -> Optional<FRTStruct>

    let frtLocalVar2 = global_function_returning_FRT_with_attr_returns_retained_wihout_Nonnull()
    // CHECK: function_ref @$sSo71global_function_returning_FRT_with_attr_returns_retained_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar3 = global_function_returning_FRT_with_attr_returns_unretained_wihout_Nonnull()
    // CHECK: function_ref @$sSo73global_function_returning_FRT_with_attr_returns_unretained_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () -> Optional<FRTStruct>

    let frtLocalVar4 = global_function_returning_copy_wihout_Nonnull()
    // CHECK: function_ref @$sSo45global_function_returning_copy_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar5 = global_function_returning_create_wihout_Nonnull()
    // CHECK: function_ref @$sSo47global_function_returning_create_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar6 = global_function_returning_copy_with_attr_returns_retained_wihout_Nonnull()
    // CHECK: function_ref @$sSo72global_function_returning_copy_with_attr_returns_retained_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () ->  @owned Optional<FRTStruct>

    let frtLocalVar7 = global_function_returning_copy_with_attr_returns_unretained_wihout_Nonnull()
    // CHECK: function_ref @$sSo74global_function_returning_copy_with_attr_returns_unretained_wihout_NonnullSo9FRTStructVSgyFTo : $@convention(c) () -> Optional<FRTStruct>
}

func testStaticMethodsWithoutAttrubutes() {
    let frtLocalVar1 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT()
    // CHECK: function_ref @$sSo52StructWithStaticMethodsReturningFRTWithoutAttributesV0c6MethodE3FRTSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_copy()
    // CHECK: function_ref @$sSo52StructWithStaticMethodsReturningFRTWithoutAttributesV0c6MethodE8FRT_copySo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar3 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_create()
    // CHECK: function_ref @$sSo52StructWithStaticMethodsReturningFRTWithoutAttributesV0c6MethodE10FRT_createSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_init()
    // CHECK: function_ref @$sSo52StructWithStaticMethodsReturningFRTWithoutAttributesV0c6MethodE8FRT_initSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = StructWithStaticMethodsReturningFRTWithoutAttributes.StaticMethodReturningFRT_clone()
    // CHECK: function_ref @$sSo52StructWithStaticMethodsReturningFRTWithoutAttributesV0c6MethodE9FRT_cloneSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct
}

func testStaticMethodsWithAttrubuteReturnsRetained() {
    let frtLocalVar1 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT()
    // CHECK: function_ref @$sSo63StructWithStaticMethodsReturningFRTWithAttributeReturnsRetainedV0c6MethodE3FRTSo9FRTStructVyFZTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_copy()
    // CHECK: function_ref @$sSo63StructWithStaticMethodsReturningFRTWithAttributeReturnsRetainedV0c6MethodE8FRT_copySo9FRTStructVyFZTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar3 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_create()
    // CHECK: function_ref @$sSo63StructWithStaticMethodsReturningFRTWithAttributeReturnsRetainedV0c6MethodE10FRT_createSo9FRTStructVyFZTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar4 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_init()
    // CHECK: function_ref @$sSo63StructWithStaticMethodsReturningFRTWithAttributeReturnsRetainedV0c6MethodE8FRT_initSo9FRTStructVyFZTo : $@convention(c) () -> @owned FRTStruct

    let frtLocalVar5 = StructWithStaticMethodsReturningFRTWithAttributeReturnsRetained.StaticMethodReturningFRT_clone()
    // CHECK: function_ref @$sSo63StructWithStaticMethodsReturningFRTWithAttributeReturnsRetainedV0c6MethodE9FRT_cloneSo9FRTStructVyFZTo : $@convention(c) () -> @owned FRTStruct
}

func testStaticMethodsWithAttrubuteReturnsUnretained() {
    let frtLocalVar1 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT()
    // CHECK: function_ref @$sSo65StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretainedV0c6MethodE3FRTSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar2 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_copy()
    // CHECK: function_ref @$sSo65StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretainedV0c6MethodE8FRT_copySo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar3 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_create()
    // CHECK: function_ref @$sSo65StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretainedV0c6MethodE10FRT_createSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar4 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_init()
    // CHECK: function_ref @$sSo65StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretainedV0c6MethodE8FRT_initSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct

    let frtLocalVar5 = StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretained.StaticMethodReturningFRT_clone()
    // CHECK: function_ref @$sSo65StructWithStaticMethodsReturningFRTWithAttributeReturnsUnretainedV0c6MethodE9FRT_cloneSo9FRTStructVyFZTo : $@convention(c) () -> FRTStruct
}

func testFreeFunctionsReturningNonFRT() {
    let frtLocalVar1 = global_function_returning_non_FRT()
    // CHECK: function_ref @$sSo33global_function_returning_non_FRTSpySo12NonFRTStructVGyFTo : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct>

    let frtLocalVar4 = global_function_returning_non_FRT_create()
    // CHECK: function_ref @$sSo40global_function_returning_non_FRT_createSpySo12NonFRTStructVGyFTo : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct>

    let frtLocalVar5 = global_function_returning_non_FRT_copy()
    // CHECK: function_ref @$sSo38global_function_returning_non_FRT_copySpySo12NonFRTStructVGyFTo : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct>
}

func testStaticMethodsReturningNonFRT() {
    let frtLocalVar1 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT()
    // CHECK: function_ref @$sSo38StructWithStaticMethodsReturningNonFRTV0c6MethodefG0SpySo0F9FRTStructVGyFZTo : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct>

    let frtLocalVar4 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT_create()
    // CHECK: function_ref @$sSo38StructWithStaticMethodsReturningNonFRTV0c6MethodefG7_createSpySo0F9FRTStructVGyFZTo : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct>

    let frtLocalVar5 = StructWithStaticMethodsReturningNonFRT.StaticMethodReturningNonFRT_copy()
    // CHECK: function_ref @$sSo38StructWithStaticMethodsReturningNonFRTV0c6MethodefG5_copySpySo0F9FRTStructVGyFZTo : $@convention(c) () -> UnsafeMutablePointer<NonFRTStruct>
}

func testtFreeFunctionsTemplated(frt : FRTStruct, nonFrt: NonFRTStruct) {
    let frtLocalVar1 : Int = 1;

    let frtLocalVar2 = global_templated_function_returning_FRT(frtLocalVar1)
    // CHECK: function_ref @$sSo39global_templated_function_returning_FRTySo9FRTStructVSiFTo : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar3 = global_templated_function_returning_FRT_copy(frtLocalVar1)
    // CHECK: function_ref @$sSo44global_templated_function_returning_FRT_copyySo9FRTStructVSiFTo : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar4 = global_templated_function_returning_FRT_create(frtLocalVar1)
    // CHECK: function_ref @$sSo46global_templated_function_returning_FRT_createySo9FRTStructVSiFTo : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar5 = global_templated_function_returning_FRT_init(frtLocalVar1)
    // CHECK: function_ref @$sSo44global_templated_function_returning_FRT_initySo9FRTStructVSiFTo : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar6 = global_templated_function_returning_FRT_clone(frtLocalVar1)
    // CHECK: function_ref @$sSo45global_templated_function_returning_FRT_cloneySo9FRTStructVSiFTo : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar7 = global_templated_function_returning_FRT_with_attr_returns_retained(frtLocalVar1)
    // CHECK: function_ref @$sSo66global_templated_function_returning_FRT_with_attr_returns_retainedySo9FRTStructVSiFTo : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar8 = global_templated_function_returning_FRT_copy_with_attr_returns_retained(frtLocalVar1)
    // CHECK: function_ref @$sSo71global_templated_function_returning_FRT_copy_with_attr_returns_retainedySo9FRTStructVSiFTo : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar9 = global_templated_function_returning_FRT_create_with_attr_returns_retained(frtLocalVar1)
    // CHECK: function_ref @$sSo73global_templated_function_returning_FRT_create_with_attr_returns_retainedySo9FRTStructVSiFTo : $@convention(c) (Int) -> @owned FRTStruct

    let frtLocalVar10 = global_templated_function_returning_FRT_with_attr_returns_unretained(frtLocalVar1)
    // CHECK: function_ref @$sSo68global_templated_function_returning_FRT_with_attr_returns_unretainedySo9FRTStructVSiFTo : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar11 = global_templated_function_returning_FRT_copy_with_attr_returns_unretained(frtLocalVar1)
    // CHECK: function_ref @$sSo73global_templated_function_returning_FRT_copy_with_attr_returns_unretainedySo9FRTStructVSiFTo : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar12 = global_templated_function_returning_FRT_create_with_attr_returns_unretained(frtLocalVar1)
    // CHECK: function_ref @$sSo75global_templated_function_returning_FRT_create_with_attr_returns_unretainedySo9FRTStructVSiFTo : $@convention(c) (Int) -> FRTStruct

    let frtLocalVar13 = global_function_returning_templated_retrun_frt(frt)
    // CHECK: function_ref @$sSo46global_function_returning_templated_retrun_frtySo9FRTStructVACFTo : $@convention(c) (FRTStruct) -> FRTStruct

    let frtLocalVar14 = global_function_returning_templated_retrun_frt_owned(frt)
    // CHECK: function_ref @$sSo52global_function_returning_templated_retrun_frt_ownedySo9FRTStructVACFTo : $@convention(c) (FRTStruct) -> @owned FRTStruct

    let nonFrtLocalVar1 = global_function_returning_templated_retrun_frt_owned(nonFrt)
    // CHECK: function_ref @$sSo52global_function_returning_templated_retrun_frt_ownedySo12NonFRTStructVACFTo : $@convention(c) (NonFRTStruct) -> NonFRTStruct
}

func testVirtualMethods(base: Base, derived: Derived) {
    var mutableBase = base
    var mutableDerived = derived

    var frt1 = mutableBase.VirtualMethodReturningFRTUnowned()
    // CHECK: function_ref @$sSo4BaseV32VirtualMethodReturningFRTUnownedSo9FRTStructVyFTo : $@convention(cxx_method) (@inout Base) -> FRTStruct

    var frt2 = mutableDerived.VirtualMethodReturningFRTUnowned()
    // CHECK: function_ref @$sSo7DerivedV32VirtualMethodReturningFRTUnownedSo9FRTStructVyFTo : $@convention(cxx_method) (@inout Derived) -> FRTStruct

    var frt3 = mutableBase.VirtualMethodReturningFRTOwned()
    // CHECK: function_ref @$sSo4BaseV30VirtualMethodReturningFRTOwnedSo9FRTStructVyFTo : $@convention(cxx_method) (@inout Base) -> @owned FRTStruct

    var frt4 = mutableDerived.VirtualMethodReturningFRTOwned()
    // CHECK: function_ref @$sSo7DerivedV30VirtualMethodReturningFRTOwnedSo9FRTStructVyFTo : $@convention(cxx_method) (@inout Derived) -> @owned FRTStruct
}

func testDefaultOwnershipAnnotation() {
  let _ = DefaultOwnershipConventionOnCXXForeignRefType.returnRefTyDefUnretained()
  // CHECK: function_ref @$sSo45DefaultOwnershipConventionOnCXXForeignRefTypeO06returnF15TyDefUnretainedAB0fijK0VyFZTo : $@convention(c) () -> DefaultOwnershipConventionOnCXXForeignRefType.RefTyDefUnretained

  let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretained()
  // CHECK: function_ref @$sSo31FunctionAnnotationHasPrecedenceO24returnRefTyDefUnretainedAB0fghI0VyFZTo : $@convention(c) () -> FunctionAnnotationHasPrecedence.RefTyDefUnretained

  let _ = FunctionAnnotationHasPrecedence.returnRefTyDefUnretainedAnnotatedRetained()
  // CHECK: function_ref @$sSo31FunctionAnnotationHasPrecedenceO41returnRefTyDefUnretainedAnnotatedRetainedAB0fghI0VyFZTo : $@convention(c) () -> @owned FunctionAnnotationHasPrecedence.RefTyDefUnretained

  let _ = DefaultOwnershipInheritance.createBaseType()
  // CHECK: function_ref @$sSo27DefaultOwnershipInheritanceO14createBaseTypeAB0eF0VyFZTo : $@convention(c) () -> DefaultOwnershipInheritance.BaseType

  let _ = DefaultOwnershipInheritance.createDerivedType()
  // CHECK: function_ref @$sSo27DefaultOwnershipInheritanceO17createDerivedTypeAB0eF0VyFZTo : $@convention(c) () -> DefaultOwnershipInheritance.DerivedType

  let _ = DefaultOwnershipInheritance.createDerivedType2()
  // CHECK: function_ref @$sSo27DefaultOwnershipInheritanceO18createDerivedType2AB0eF0VyFZTo : $@convention(c) () -> DefaultOwnershipInheritance.DerivedType2

  let _ = DefaultOwnershipInheritance.createBaseTypeNonDefault()
  // CHECK: function_ref @$sSo27DefaultOwnershipInheritanceO017createBaseTypeNonA0AB0efgA0VyFZTo : $@convention(c) () -> @owned DefaultOwnershipInheritance.BaseTypeNonDefault

  let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefault()
  // CHECK: function_ref @$sSo27DefaultOwnershipInheritanceO020createDerivedTypeNonA0AB0efgA0VyFZTo : $@convention(c) () -> @owned DefaultOwnershipInheritance.DerivedTypeNonDefault

  let _ = DefaultOwnershipInheritance.createDerivedTypeNonDefaultUnretained()
  // CHECK: function_ref @$sSo27DefaultOwnershipInheritanceO020createDerivedTypeNonA10UnretainedAB0efgA0VyFZTo : $@convention(c) () -> DefaultOwnershipInheritance.DerivedTypeNonDefault
}

func testTemplateMemberFunctions() {
  let refTemplate = FRTStructRef()

  let ptr = refTemplate.ptr()
  // CHECK: function_ref @$sSo0028RefTemplateFRTStruct_eBAGgsbV3ptrSo9FRTStructVyFTo : $@convention(cxx_method) (@in_guaranteed RefTemplate<FRTStruct>) -> @owned FRTStruct

  let get = refTemplate.get()
  // CHECK: function_ref @$sSo0028RefTemplateFRTStruct_eBAGgsbV3getSo9FRTStructVyFTo : $@convention(cxx_method) (@in_guaranteed RefTemplate<FRTStruct>) -> FRTStruct

  let value = refTemplate.value()
  // CHECK: function_ref @$sSo0028RefTemplateFRTStruct_eBAGgsbV5valueSo9FRTStructVyFTo : $@convention(cxx_method) (@in_guaranteed RefTemplate<FRTStruct>) -> FRTStruct
}
