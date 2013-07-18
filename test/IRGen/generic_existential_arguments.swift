// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm | FileCheck %s

protocol Foo { func foo() }
protocol Bar { func bar() }

protocol [class_protocol] ClassFoo { func foo() }
protocol [class_protocol] ClassBar { func bar() }

/* FIXME: We currently cannot distinguish between a protocol type substituting
 * directly for an archetype and an any type's contained type substituting a 
 * generic.
func genericAny<T>(x:T) { }
func existentialAny(x:protocol<>) { genericAny(x) }
 */

func genericFoo<T:Foo>(x:T) { }
func existentialFoo(x:Foo) { genericFoo(x) }
// CHECK: define void @_T29generic_existential_arguments14existentialFooFT1xPS_3Foo__T_([[FOO_T:%P29generic_existential_arguments3Foo_]]*) {
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_T]]* [[CONTAINER:%.*]], i32 0, i32 2
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_T]]* [[CONTAINER:%.*]], i32 0, i32 2
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_T]]* [[CONTAINER:%.*]], i32 0, i32 2
// CHECK:   [[VALUE:%.*]] = call %swift.opaque* %projectBuffer([24 x i8]* [[BUFFER]], %swift.type* {{%.*}})
// CHECK:   [[METADATA_PTR:%.*]] = getelementptr inbounds [[FOO_T]]* [[CONTAINER]], i32 0, i32 0
// CHECK:   [[METADATA:%.*]] = load %swift.type** [[METADATA_PTR]], align 8
// CHECK:   [[WITNESS_PTR:%.*]] = getelementptr inbounds [[FOO_T]]* [[CONTAINER]], i32 0, i32 1
// CHECK:   [[WITNESS:%.*]] = load i8*** [[WITNESS_PTR]], align 8
// CHECK:   call void @_T29generic_existential_arguments10genericFooUS_3Foo__FT1xQ__T_(%swift.opaque* [[VALUE]], %swift.type* [[METADATA]], i8** [[WITNESS]])

func genericFooBar<T:protocol<Foo,Bar>>(x:T) { }
func existentialFooBar(x:protocol<Foo,Bar>) { genericFooBar(x) }
// CHECK: define void @_T29generic_existential_arguments17existentialFooBarFT1xPS_3BarS_3Foo__T_([[FOO_BAR_T:%"protocol<_T29generic_existential_arguments3Bar,_T29generic_existential_arguments3Foo>"]]*) {
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER:%.*]], i32 0, i32 3
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER:%.*]], i32 0, i32 3
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER:%.*]], i32 0, i32 3
// CHECK:   [[VALUE:%.*]] = call %swift.opaque* %projectBuffer([24 x i8]* [[BUFFER]], %swift.type* {{%.*}})
// CHECK:   [[METADATA_PTR:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER]], i32 0, i32 0
// CHECK:   [[METADATA:%.*]] = load %swift.type** [[METADATA_PTR]], align 8
// CHECK:   [[BAR_WITNESS_PTR:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER]], i32 0, i32 1
// CHECK:   [[BAR_WITNESS:%.*]] = load i8*** [[BAR_WITNESS_PTR]], align 8
// CHECK:   [[FOO_WITNESS_PTR:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER]], i32 0, i32 2
// CHECK:   [[FOO_WITNESS:%.*]] = load i8*** [[FOO_WITNESS_PTR]], align 8
// CHECK:   call void @_T29generic_existential_arguments13genericFooBarUS_3BarS_3Foo__FT1xQ__T_(%swift.opaque* [[VALUE]], %swift.type* [[METADATA]], i8** [[BAR_WITNESS]], i8** [[FOO_WITNESS]])
func existentialFooBarFoo(x:protocol<Foo,Bar>) { genericFoo(x) }
// CHECK: define void @_T29generic_existential_arguments20existentialFooBarFooFT1xPS_3BarS_3Foo__T_(%"protocol<_T29generic_existential_arguments3Bar,_T29generic_existential_arguments3Foo>"*) {
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER:%.*]], i32 0, i32 3
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER:%.*]], i32 0, i32 3
// CHECK:   [[BUFFER:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER:%.*]], i32 0, i32 3
// CHECK:   [[VALUE:%.*]] = call %swift.opaque* %projectBuffer([24 x i8]* [[BUFFER]], %swift.type* {{%.*}})
// CHECK:   [[METADATA_PTR:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER]], i32 0, i32 0
// CHECK:   [[METADATA:%.*]] = load %swift.type** [[METADATA_PTR]], align 8
// CHECK:   [[FOO_WITNESS_PTR:%.*]] = getelementptr inbounds [[FOO_BAR_T]]* [[CONTAINER]], i32 0, i32 2
// CHECK:   [[FOO_WITNESS:%.*]] = load i8*** [[FOO_WITNESS_PTR]], align 8
// CHECK:   call void @_T29generic_existential_arguments10genericFooUS_3Foo__FT1xQ__T_(%swift.opaque* [[VALUE]], %swift.type* [[METADATA]], i8** [[FOO_WITNESS]])

func genericClassFoo<T:ClassFoo>(x:T) { }
func existentialClassFoo(x:ClassFoo) { genericClassFoo(x) }
// CHECK: define void @_T29generic_existential_arguments19existentialClassFooFT1xPS_8ClassFoo__T_(i8**, %objc_object*) {
// CHECK:   [[WITNESS_PTR:%.*]] = getelementptr inbounds %P29generic_existential_arguments8ClassFoo_* [[X:%x]], i32 0, i32 0
// CHECK:   [[WITNESS_PTR:%.*]] = getelementptr inbounds %P29generic_existential_arguments8ClassFoo_* [[X:%x]], i32 0, i32 0
// CHECK:   [[WITNESS:%.*]] = load i8*** [[WITNESS_PTR]], align 8
// CHECK:   [[OBJECT_PTR:%.*]] = getelementptr inbounds %P29generic_existential_arguments8ClassFoo_* [[X]], i32 0, i32 1
// CHECK:   [[OBJECT:%.*]] = load %objc_object** [[OBJECT_PTR]], align 8
// CHECK:   [[METADATA:%.*]] = call %swift.type* @swift_getObjectType(%objc_object* [[OBJECT]])
// CHECK:   call void @_T29generic_existential_arguments15genericClassFooUS_8ClassFoo__FT1xQ__T_(%objc_object* [[OBJECT]], %swift.type* [[METADATA]], i8** [[WITNESS]])

