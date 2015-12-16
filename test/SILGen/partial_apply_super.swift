// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name OutsideClasses -o %t %S/../Inputs/outside_classes_before.swift
// RUN: %target-swift-frontend -use-native-super-method -emit-silgen -I %t %S/../Inputs/partial_apply_super.swift | FileCheck %s --check-prefix=SILGEN

// Child.method
// SILGEN-LABEL: sil hidden @_TFC19partial_apply_super5Child6methodfT_T_
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $Child to $Parent
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $Child, #Parent.method!1
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]])
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]])

// Child.classMethod
// SILGEN-LABEL: sil hidden @_TZFC19partial_apply_super5Child11classMethodfT_T_ : $@convention(thin) (@thick Child.Type) -> () {
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> () // user: %6
// SILGEN-NEXT: [[CASTED_SELF:%[0-9]+]] = upcast %0 : $@thick Child.Type to $@thick Parent.Type // user: %5
// SILGEN-NEXT: [[SUPER_METHOD:%[0-9]+]] = super_method %0 : $@thick Child.Type, #Parent.classMethod!1 : Parent.Type -> () -> () , $@convention(thin) (@thick Parent.Type) -> ()
// SILGEN-NEXT: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> ()
// SILGEN-NEXT:  apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

// GenericChild.method
// SILGEN-LABEL: sil hidden @_TFC19partial_apply_super12GenericChild6methodfT_T_ : $@convention(method) <A> (@guaranteed GenericChild<A>) -> () 
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $GenericChild<A> to $GenericParent<A>
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method {{%[0-9]+}} : $GenericChild<A>, #GenericParent.method!1 : <A> GenericParent<A> -> () -> () , $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

// GenericChild.classMethod
// SILGEN-LABEL: sil hidden @_TZFC19partial_apply_super12GenericChild11classMethodfT_T_ : $@convention(thin) <A> (@thick GenericChild<A>.Type) -> ()
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $@thick GenericChild<A>.Type to $@thick GenericParent<A>.Type
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method {{%[0-9]}} : $@thick GenericChild<A>.Type, #GenericParent.classMethod!1 : <A> GenericParent<A>.Type -> () -> () , $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

// closure.Child.method
// SILGEN-LABEL: sil shared @_TFCF19partial_apply_superU_FT_T_L_5Child6methodfT_T_ : $@convention(method) (@guaranteed Child) -> ()
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> () // user: %7
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $Child to $Parent
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method {{%[0-9]+}} : $Child, #Parent.method!1 : Parent -> () -> () , $@convention(method) (@guaranteed Parent) -> ()
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(method) (@guaranteed Parent) -> ()
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

// closure.Child.classMethod
// SILGEN-LABEL: sil shared @_TZFCF19partial_apply_superU_FT_T_L_5Child11classMethodfT_T_ : $@convention(thin) (@thick Child.Type) -> () {
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> () // user: %6
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $@thick Child.Type to $@thick Parent.Type // user: %5
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method {{%[0-9]+}} : $@thick Child.Type, #Parent.classMethod!1 : Parent.Type -> () -> () , $@convention(thin) (@thick Parent.Type) -> () // user: %5
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]([[CASTED_SELF]]) : $@convention(thin) (@thick Parent.Type) -> () // user: %6
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

// closure.GenericChild.method
// SILGEN-LABEL: sil shared @_TFCF19partial_apply_superU0_FT_T_L_12GenericChild6methodfT_T_ : $@convention(method) <A> (@guaranteed GenericChild<A>) -> ()
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $GenericChild<A> to $GenericParent<A>
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method {{%[0-9]+}} : $GenericChild<A>, #GenericParent.method!1 : <A> GenericParent<A> -> () -> () , $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(method) <τ_0_0> (@guaranteed GenericParent<τ_0_0>) -> ()
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()

// closure.GenericChild.classMethod
// SILGEN-LABEL: sil shared @_TZFCF19partial_apply_superU0_FT_T_L_12GenericChild11classMethodfT_T_ : $@convention(thin) <A> (@thick GenericChild<A>.Type) -> ()
// SILGEN: [[DOFOO:%[0-9]+]] = function_ref @_TF14OutsideClasses5doFooFFT_T_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
// SILGEN: [[CASTED_SELF:%[0-9]+]] = upcast {{%[0-9]+}} : $@thick GenericChild<A>.Type to $@thick GenericParent<A>.Type
// SILGEN: [[SUPER_METHOD:%[0-9]+]] = super_method {{%[0-9]+}} : $@thick GenericChild<A>.Type, #GenericParent.classMethod!1 : <A> GenericParent<A>.Type -> () -> () , $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
// SILGEN: [[PARTIAL_APPLY:%[0-9]+]] = partial_apply [[SUPER_METHOD]]<A>([[CASTED_SELF]]) : $@convention(thin) <τ_0_0> (@thick GenericParent<τ_0_0>.Type) -> ()
// SILGEN: apply [[DOFOO]]([[PARTIAL_APPLY]]) : $@convention(thin) (@owned @callee_owned () -> ()) -> ()
