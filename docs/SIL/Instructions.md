# Instruction Set

This document is a reference guide of the SIL instruction set. For an overview
of SIL and OSSA see the [SIL](SIL.md) document.

## Allocation and Deallocation

These instructions allocate and deallocate memory.

### alloc_stack

```
sil-instruction ::= 'alloc_stack' alloc-stack-option* sil-type (',' debug-var-attr)*
alloc-stack-option ::= '[dynamic_lifetime]'
alloc-stack-option ::= '[lexical]'
alloc-stack-option ::= '[var_decl]'
alloc-stack-option ::= '[moveable_value_debuginfo]'

%1 = alloc_stack $T
// %1 has type $*T
```

Allocates uninitialized memory that is sufficiently aligned on the stack
to contain a value of type `T`. The result of the instruction is the
address of the allocated memory.

`alloc_stack` always allocates memory on the stack even for
runtime-sized type.

`alloc_stack` is a stack allocation instruction. See the section above
on stack discipline. The corresponding stack deallocation instruction is
`dealloc_stack`.

The `dynamic_lifetime` attribute specifies that the initialization and
destruction of the stored value cannot be verified at compile time. This
is the case, e.g. for conditionally initialized objects.

The optional `lexical` attribute specifies that the operand corresponds
to a local variable with a lexical lifetime in the Swift source, so
special care must be taken when hoisting `destroy_addr`s. Compare to the
`var_decl` attribute. See [Variable Lifetimes](Ownership.md#variable-lifetimes).

The optional `var_decl` attribute specifies that the storage corresponds
to a local variable in the Swift source.

The optional `moveable_value_debuginfo` attribute specifies that when
emitting debug info, the code generator can not assume that the value in
the alloc_stack can be semantically valid over the entire function frame
when emitting debug info. NOTE: This is implicitly set to true if the
alloc_stack's type is non-copyable. This is just done to make SIL less
verbose.

The memory is not retainable. To allocate a retainable box for a value
type, use `alloc_box`.

`T` must not be a pack type. To allocate a pack, use `alloc_pack`.

### alloc_pack

```
sil-instruction ::= 'alloc_pack' sil-type

%1 = alloc_pack $Pack{Int, Float, repeat each T}
// %1 has type $*Pack{Int, Float, repeat each T}
```

Allocates uninitialized memory on the stack for a value pack of the
given type, which must be a pack type. The result of the instruction is
the address of the allocated memory.

`alloc_pack` is a stack allocation instruction. See the section above on
stack discipline. The corresponding stack deallocation instruction is
`dealloc_pack`.

### alloc_pack_metadata

```
sil-instruction ::= 'alloc_pack_metadata' $()
```

Inserted as the last SIL lowering pass of IRGen, indicates that the next
instruction may have on-stack pack metadata allocated on its behalf.

Notionally, `alloc_pack_metadata` is a stack allocation instruction. See
the section above on stack discipline. The corresponding stack
deallocation instruction is `dealloc_pack_metadata`.

Only valid in Lowered SIL.

### alloc_ref

```
sil-instruction ::= 'alloc_ref'
                      ('[' 'bare' ']')?
                      ('[' 'objc' ']')?
                      ('[' 'stack' ']')?
                      ('[' 'tail_elems' sil-type '*' sil-operand ']')*
                      sil-type

%1 = alloc_ref [stack] $T
%1 = alloc_ref [tail_elems $E * %2 : Builtin.Word] $T
// $T must be a reference type
// %1 has type $T
// $E is the type of the tail-allocated elements
// %2 must be of a builtin integer type
```

Allocates an object of reference type `T`. The object will be
initialized with retain count 1; its state will be otherwise
uninitialized. The optional `objc` attribute indicates that the object
should be allocated using Objective-C's allocation methods
(`+allocWithZone:`).

The optional `stack` attribute indicates that the object can be
allocated on the stack instead on the heap. In this case the instruction
must be balanced with a `dealloc_stack_ref` instruction to mark the end
of the object's lifetime. Note that the `stack` attribute only
specifies that stack allocation is possible. The final decision on stack
allocation is done during llvm IR generation. This is because the
decision also depends on the object size, which is not necessarily known
at SIL level.

The `bare` attribute indicates that the object header is not used
throughout the lifetime of the object. This means, no reference counting
operations are performed on the object and its metadata is not used. The
header of bare objects doesn't need to be initialized.

The optional `tail_elems` attributes specifies the amount of space to be
reserved for tail-allocated arrays of given element types and element
counts. If there are more than one `tail_elems` attributes then the tail
arrays are allocated in the specified order. The count-operand must be
of a builtin integer type. The instructions `ref_tail_addr` and
`tail_addr` can be used to project the tail elements. The `objc`
attribute cannot be used together with `tail_elems`.

### alloc_ref_dynamic

```
sil-instruction ::= 'alloc_ref_dynamic'
                      ('[' 'objc' ']')?
                      ('[' 'tail_elems' sil-type '*' sil-operand ']')*
                      sil-operand ',' sil-type

%1 = alloc_ref_dynamic %0 : $@thick T.Type, $T
%1 = alloc_ref_dynamic [objc] %0 : $@objc_metatype T.Type, $T
%1 = alloc_ref_dynamic [tail_elems $E * %2 : Builtin.Word] %0 : $@thick T.Type, $T
// $T must be a class type
// %1 has type $T
// $E is the type of the tail-allocated elements
// %2 must be of a builtin integer type
```

Allocates an object of class type `T` or a subclass thereof. The dynamic
type of the resulting object is specified via the metatype value `%0`.
The object will be initialized with retain count 1; its state will be
otherwise uninitialized.

The optional `tail_elems` and `objc` attributes have the same effect as
for `alloc_ref`. See `alloc_ref` for details.

### alloc_box

```
sil-instruction ::= 'alloc_box' alloc-box-option* sil-type (',' debug-var-attr)*
alloc-box-option ::= moveable_value_debuginfo

%1 = alloc_box $T
//   %1 has type $@box T
```

Allocates a reference-counted `@box` on the heap large enough to hold a
value of type `T`, along with a retain count and any other metadata
required by the runtime. The result of the instruction is the
reference-counted `@box` reference that owns the box. The `project_box`
instruction is used to retrieve the address of the value inside the box.

The box will be initialized with a retain count of 1; the storage will
be uninitialized. The box owns the contained value, and releasing it to
a retain count of zero destroys the contained value as if by
`destroy_addr`. Releasing a box is undefined behavior if the box's
value is uninitialized. To deallocate a box whose value has not been
initialized, `dealloc_box` should be used.

The optional `moveable_value_debuginfo` attribute specifies that when
emitting debug info, the code generator can not assume that the value in
the alloc_stack can be semantically valid over the entire function frame
when emitting debug info. NOTE: This is implicitly set to true if the
alloc_stack's type is noncopyable. This is just done to make SIL less
verbose.

### alloc_global

```
sil-instruction ::= 'alloc_global' sil-global-name

alloc_global @foo
```

Initialize the storage for a global variable. This instruction has
undefined behavior if the global variable has already been initialized.

The type operand must be a lowered object type.

### get_async_continuation

```
sil-instruction ::= 'get_async_continuation' '[throws]'? sil-type

%0 = get_async_continuation $T
%0 = get_async_continuation [throws] $U
```

Begins a suspension of an `@async` function. This instruction can only
be used inside an `@async` function. The result of the instruction is an
`UnsafeContinuation<T>` value, where `T` is the formal type argument to
the instruction, or an `UnsafeThrowingContinuation<T>` if the
instruction carries the `[throws]` attribute. `T` must be a loadable
type. The continuation must be consumed by a `await_async_continuation`
terminator on all paths. Between `get_async_continuation` and
`await_async_continuation`, the following restrictions apply:

-   The function cannot `return`, `throw`, `yield`, or `unwind`.
-   There cannot be nested suspend points; namely, the function cannot
    call another `@async` function, nor can it initiate another suspend
    point with `get_async_continuation`.

The function suspends execution when the matching
`await_async_continuation` terminator is reached, and resumes execution
when the continuation is resumed. The continuation resumption operation
takes a value of type `T` which is passed back into the function when it
resumes execution in the `await_async_continuation` instruction's
`resume` successor block. If the instruction has the `[throws]`
attribute, it can also be resumed in an error state, in which case the
matching `await_async_continuation` instruction must also have an
`error` successor.

Within the enclosing SIL function, the result continuation is consumed
by the `await_async_continuation`, and cannot be referenced after the
`await_async_continuation` executes. Dynamically, the continuation value
must be resumed exactly once in the course of the program's execution;
it is undefined behavior to resume the continuation more than once.
Conversely, failing to resume the continuation will leave the suspended
async coroutine hung in its suspended state, leaking any resources it
may be holding.

### get_async_continuation_addr

```
sil-instruction ::= 'get_async_continuation_addr' '[throws]'? sil-type ',' sil-operand

%1 = get_async_continuation_addr $T, %0 : $*T
%1 = get_async_continuation_addr [throws] $U, %0 : $*U
```

Begins a suspension of an `@async` function, like
`get_async_continuation`, additionally binding a specific memory
location for receiving the value when the result continuation is
resumed. The operand must be an address whose type is the
maximally-abstracted lowered type of the formal resume type. The memory
must be uninitialized, and must remain allocated until the matching
`await_async_continuation` instruction(s) consuming the result
continuation have executed. The behavior is otherwise the same as
`get_async_continuation`, and the same restrictions apply on code
appearing between `get_async_continuation_addr` and
`await_async_continuation` as apply between `get_async_continuation` and
`await_async_continuation`. Additionally, the state of the memory
referenced by the operand is indefinite between the execution of
`get_async_continuation_addr` and `await_async_continuation`, and it is
undefined behavior to read or modify the memory during this time. After
the `await_async_continuation` resumes normally to its `resume`
successor, the memory referenced by the operand is initialized with the
resume value, and that value is then owned by the current function. If
`await_async_continuation` instead resumes to its `error` successor,
then the memory remains uninitialized.

### hop_to_executor

```
sil-instruction ::= 'hop_to_executor' sil-operand

hop_to_executor %0 : $T

// $T must be Builtin.Executor or conform to the Actor protocol
```

Ensures that all instructions, which need to run on the actor's
executor actually run on that executor. This instruction can only be
used inside an `@async` function.

Checks if the current executor is the one which is bound to the operand
actor. If not, begins a suspension point and enqueues the continuation
to the executor which is bound to the operand actor.

SIL generation emits this instruction with operands of actor type as
well as of type `Builtin.Executor`. The former are expected to be
lowered by the SIL pipeline, so that IR generation only operands of type
`Builtin.Executor` remain.

The operand is a guaranteed operand, i.e. not consumed.

### extract_executor

```
sil-instruction ::= 'extract_executor' sil-operand

%1 = extract_executor %0 : $T
// $T must be Builtin.Executor or conform to the Actor protocol
// %1 will be of type Builtin.Executor
```

Extracts the executor from the executor or actor operand. SIL generation
emits this instruction to produce executor values when needed (e.g., to
provide to a runtime function). It will be lowered away by the SIL
pipeline.

The operand is a guaranteed operand, i.e. not consumed.

### merge_isolation_region

```
sil-instruction :: 'merge_isolation_region' (sil-operand ',')+ sil-operand

%2 = merge_isolation_region %first : $*T, %second : $U
%2 = merge_isolation_region %first : $*T, %second : $U, %third : $H
```

Instruction that is only valid in Ownership SSA.

This instruction informs region isolation that all of the operands
should be considered to be artificially apart of the same region. It is
intended to be used to express region dependency when due to unsafe
code generation we have to traffic a non-Sendable value through computations
with Sendable values (causing us to not track the non-Sendable value)
but have to later express that a non-Sendable result of using the
Sendable value needs to be in the same region as the original
non-Sendable value. As an example of where this comes up, consider the
following code:

```
// objc code
@interface CallbackData : NSObject
@end

@interface Klass : NSObject

- (void)loadDataWithCompletionHandler:(void (^)(CallbackData * _Nullable, NSError * _Nullable))completionHandler;

@end

// swift code
extension Klass {
  func loadCallbackData() async throws -> sending CallbackData {
    try await loadData()
  }
}
```

This lowers to:

```
%5 = alloc_stack $CallbackData                  // users: %26, %25, %31, %16, %7
%6 = objc_method %0 : $Klass, #Klass.loadData!foreign : (Klass) -> () async throws -> CallbackData, $@convention(objc_method) (Optional<@convention(block) (Optional<CallbackData>, Optional<NSError>) -> ()>, Klass) -> () // user: %20
%7 = get_async_continuation_addr [throws] CallbackData, %5 : $*CallbackData // users: %23, %8
%8 = struct $UnsafeContinuation<CallbackData, any Error> (%7 : $Builtin.RawUnsafeContinuation) // user: %14
%9 = alloc_stack $@block_storage Any            // users: %22, %16, %10
%10 = project_block_storage %9 : $*@block_storage Any // user: %11
%11 = init_existential_addr %10 : $*Any, $CheckedContinuation<CallbackData, any Error> // user: %15
// function_ref _createCheckedThrowingContinuation<A>(_:)
%12 = function_ref @$ss34_createCheckedThrowingContinuationyScCyxs5Error_pGSccyxsAB_pGnlF : $@convention(thin) <τ_0_0> (UnsafeContinuation<τ_0_0, any Error>) -> @out CheckedContinuation<τ_0_0, any Error> // user: %14
%13 = alloc_stack $CheckedContinuation<CallbackData, any Error> // users: %21, %15, %14
%14 = apply %12<CallbackData>(%13, %8) : $@convention(thin) <τ_0_0> (UnsafeContinuation<τ_0_0, any Error>) -> @out CheckedContinuation<τ_0_0, any Error>
copy_addr [take] %13 to [init] %11 : $*CheckedContinuation<CallbackData, any Error> // id: %15
merge_isolation_region %9 : $*@block_storage Any, %5 : $*CallbackData // id: %16
// function_ref @objc completion handler block implementation for @escaping @callee_unowned @convention(block) (@unowned CallbackData?, @unowned NSError?) -> () with result type CallbackData
%17 = function_ref @$sSo12CallbackDataCSgSo7NSErrorCSgIeyByy_ABTz_ : $@convention(c) (@inout_aliasable @block_storage Any, Optional<CallbackData>, Optional<NSError>) -> () // user: %18
%18 = init_block_storage_header %9 : $*@block_storage Any, invoke %17 : $@convention(c) (@inout_aliasable @block_storage Any, Optional<CallbackData>, Optional<NSError>) -> (), type $@convention(block) (Optional<CallbackData>, Optional<NSError>) -> () // user: %19
%19 = enum $Optional<@convention(block) (Optional<CallbackData>, Optional<NSError>) -> ()>, #Optional.some!enumelt, %18 : $@convention(block) (Optional<CallbackData>, Optional<NSError>) -> () // user: %20
%20 = apply %6(%19, %0) : $@convention(objc_method) (Optional<@convention(block) (Optional<CallbackData>, Optional<NSError>) -> ()>, Klass) -> ()
```

Notice how without the [merge_isolation_region](#merge_isolation_region)
instruction (`%16`) there is no non-Sendable def-use chain from `%5`, the
indirect return value of the block, to the actual non-Sendable block
storage `%9`. This can result in region isolation not propagating
restrictions on usage from `%9` onto `%5` risking the creation of races.

Applying the previous discussion to this specific example, self (`%0`) is
non-Sendable and is bound to the current task. If we did not have the
[merge_isolation_region](#merge_isolation_region) instruction here, we
would not tie the return value `%5` to `%0` via `%9`. This would cause `%5` to
be treated as a disconnected value and thus be a valid sending return
value potentially allowing for `%5` in the caller of the function to be
sent to another isolation domain and introduce a race.

> **_Note:_** This is effectively the same purpose that
[mark_dependence](#mark_dependence) plays for memory dependence
(expressing memory dependence that the compiler cannot infer) except in
the world of region isolation. We purposely use a different instruction
since [mark_dependence](#mark_dependence) is often times used to create
a temporary dependence in between two values via the return value of
[mark_dependence](#mark_dependence). If
[mark_dependence](#mark_dependence) had the semantics of acting like a
region merge we would in contrast have from that point on a region
dependence in between the base and value of the
[mark_dependence](#mark_dependence) causing the
[mark_dependence](#mark_dependence) to have a less "local" effect
since all paths through that program point would have to maintain that
region dependence until the end of the function.

### dealloc_stack

```
sil-instruction ::= 'dealloc_stack' sil-operand

dealloc_stack %0 : $*T
// %0 must be of $*T type
```

Deallocates memory previously allocated by `alloc_stack`. The allocated
value in memory must be uninitialized or destroyed prior to being
deallocated.

`dealloc_stack` is a stack deallocation instruction. See the section on
Stack Discipline above. The operand must be an `alloc_stack`
instruction.

### dealloc_pack

```
sil-instruction ::= 'dealloc_pack' sil-operand

dealloc_pack %0 : $*Pack{Int, Float, repeat each T}
// %0 must be the result of `alloc_pack $Pack{Int, Float, repeat each T}`
```

Deallocates memory for a pack value previously allocated by
`alloc_pack`. If the pack elements are direct, they must be
uninitialized or destroyed prior to being deallocated.

`dealloc_pack` is a stack deallocation instruction. See the section on
Stack Discipline above. The operand must be an `alloc_pack` instruction.

### dealloc_pack_metadata

```
sil-instruction ::= 'dealloc_pack_metadata' sil-operand

dealloc_pack_metadata $0 : $*()
```

Inserted as the last SIL lowering pass of IRGen, indicates that the
on-stack pack metadata emitted on behalf of its operand (actually on
behalf of the instruction after its operand) must be cleaned up here.

`dealloc_pack_metadata` is a stack deallocation instruction. See the
section on Stack Discipline above. The operand must be an
`alloc_pack_metadata` instruction.

Only valid in Lowered SIL.

### dealloc_box

```
sil-instruction ::= 'dealloc_box' '[dead_end]'? sil-operand

dealloc_box %0 : $@box T
```

Deallocates a box, bypassing the reference counting mechanism. The box
variable must have a retain count of one. The boxed type must match the
type passed to the corresponding `alloc_box` exactly, or else undefined
behavior results.

This does not destroy the boxed value. The contents of the value must
have been fully uninitialized or destroyed before `dealloc_box` is
applied.

The optional `dead_end` attribute specifies that this instruction was
created during lifetime completion and is eligible for deletion during
OSSA lowering.

### project_box

```
sil-instruction ::= 'project_box' sil-operand

%1 = project_box %0 : $@box T

// %1 has type $*T
```

Given a `@box T` reference, produces the address of the value inside the
box.

### dealloc_stack_ref

```
sil-instruction ::= 'dealloc_stack_ref' sil-operand

dealloc_stack_ref %0 : $T
// $T must be a class type
// %0 must be an 'alloc_ref [stack]' instruction
```

Marks the deallocation of the stack space for an `alloc_ref [stack]`.

### dealloc_ref

```
sil-instruction ::= 'dealloc_ref' sil-operand

dealloc_ref %0 : $T
// $T must be a class type
```

Deallocates an uninitialized class type instance, bypassing the
reference counting mechanism.

The type of the operand must match the allocated type exactly, or else
undefined behavior results.

The instance must have a retain count of one.

This does not destroy stored properties of the instance. The contents of
stored properties must be fully uninitialized at the time `dealloc_ref`
is applied.

The `stack` attribute indicates that the instruction is the balanced
deallocation of its operand which must be a `alloc_ref [stack]`. In this
case the instruction marks the end of the object's lifetime but has no
other effect.

### dealloc_partial_ref

```
sil-instruction ::= 'dealloc_partial_ref' sil-operand sil-metatype

dealloc_partial_ref %0 : $T, %1 : $U.Type
// $T must be a class type
// $T must be a subclass of U
```

Deallocates a partially-initialized class type instance, bypassing the
reference counting mechanism.

The type of the operand must be a supertype of the allocated type, or
else undefined behavior results.

The instance must have a retain count of one.

All stored properties in classes more derived than the given metatype
value must be initialized, and all other stored properties must be
uninitialized. The initialized stored properties are destroyed before
deallocating the memory for the instance.

This does not destroy the reference type instance. The contents of the
heap object must have been fully uninitialized or destroyed before
`dealloc_ref` is applied.

## Debug Information

Debug information is generally associated with allocations (alloc_stack
or alloc_box) by having a Decl node attached to the allocation with a
SILLocation. For declarations that have no allocation we have explicit
instructions for doing this. This is used by 'let' declarations, which
bind a value to a name and for var decls who are promoted into
registers. The decl they refer to is attached to the instruction with a
SILLocation.

### debug_value

```
sil-instruction ::= debug_value sil-debug-value-option* sil-operand (',' debug-var-attr)* advanced-debug-var-attr* (',' 'expr' debug-info-expr)?
sil-debug-value-option ::= [poison]
sil-debug-value-option ::= [moveable_value_debuginfo]
sil-debug-value-option ::= [trace]

debug_value %1 : $Int
```

This indicates that the value of a declaration has changed value to the
specified operand. The declaration in question is identified by either
the SILLocation attached to the debug_value instruction or the
SILLocation specified in the advanced debug variable attributes.

If the `moveable_value_debuginfo` flag is set, then one knows that the
debug_value's operand is moved at some point of the program, so one can
not model the debug_value using constructs that assume that the value is
live for the entire function (e.x.: llvm.dbg.declare). NOTE: This is
implicitly set to true if the alloc_stack's type is noncopyable. This
is just done to make SIL less verbose.

```
debug-var-attr ::= 'var'
debug-var-attr ::= 'let'
debug-var-attr ::= 'name' string-literal
debug-var-attr ::= 'argno' integer-literal
```

There are a number of attributes that provide details about the source
variable that is being described, including the name of the variable.
For function and closure arguments `argno` is the number of the function
argument starting with 1. A compiler-generated source variable will be
marked `implicit` and optimizers are free to remove it even in -Onone.

If the '[poison]' flag is set, then all references within this debug
value will be overwritten with a sentinel at this point in the program.
This is used in debug builds when shortening non-trivial value lifetimes
to ensure the debugger cannot inspect invalid memory. `debug_value`
instructions with the poison flag are not generated until OSSA is
lowered. They are not expected to be serialized within the module, and
the pipeline is not expected to do any significant code motion after
lowering.

```
advanced-debug-var-attr ::= '(' 'name' string-literal (',' sil-instruction-source-info)? ')'
advanced-debug-var-attr ::= 'type' sil-type
```

Advanced debug variable attributes represent source locations and the
type of the source variable when it was originally declared. It is
useful when we're indirectly associating the SSA value with the source
variable (via SIL DIExpression, for example) in which case SSA value's
type is different from that of source variable.

```
debug-info-expr   ::= di-expr-operand (':' di-expr-operand)*
di-expr-operand   ::= di-expr-operator (':' sil-operand)*
di-expr-operator  ::= 'op_fragment'
di-expr-operator  ::= 'op_tuple_fragment'
di-expr-operator  ::= 'op_deref'
```

SIL debug info expression (SIL DIExpression) is a powerful method to
connect SSA value with the source variable in an indirect fashion.
Di-expression in SIL uses a stack based execution model to evaluate the
expression and apply on the associated (SIL) SSA value before connecting
it with the debug variable. For instance, given the following SIL code:

```
debug_value %a : $*Int, name "x", expr op_deref
```

It means: "You can get the value of source variable 'x' by
*dereferencing* SSA value `%a`". The `op_deref` is a SIL DIExpression
operator that represents "dereference". If there are multiple SIL
DIExpression operators (or arguments), they are evaluated from left to
right:

```
debug_value %b : $**Int, name "y", expr op_deref:op_deref
```

In the snippet above, two `op_deref` operators will be applied on SSA
value `%b` sequentially.

Note that normally when the SSA value has an address type, there will be
a `op_deref` in the SIL DIExpression. Because there is no pointer in
Swift so you always need to dereference an address-type SSA value to get
the value of a source variable. However, if the SSA value is a
`alloc_stack`, the `debug_value` is used to indicate the *declaration*
of a source variable. Or, you can say, used to specify the location
(memory address) of the source variable. Therefore, we don't need to
add a `op_deref` in this case:

```
%a = alloc_stack $Int, ...
debug_value %a : $*Int, name "my_var"
```

The `op_fragment` operator is used to specify the SSA value of a
specific field in an aggregate-type source variable. This SIL
DIExpression operator takes a field declaration - which references the
desired sub-field in source variable - as its argument. Here is an
example:

```
struct MyStruct {
  var x: Int
  var y: Int
}
...
debug_value %1 : $Int, var, (name "the_struct", loc "file.swift":8:7), type $MyStruct, expr op_fragment:#MyStruct.y, loc "file.swift":9:4
```

In the snippet above, source variable "the_struct" has an aggregate
type `$MyStruct` and we use a SIL DIExpression with `op_fragment`
operator to associate `%1` to the `y` member variable (via the
`#MyStruct.y` directive) inside "the_struct". Note that the extra
source location directive follows right after `name "the_struct"`
indicate that "the_struct" was originally declared in line 8, but not
until line 9 - the current `debug_value` instruction's source
location - does member `y` got updated with SSA value `%1`.

For tuples, it works similarly, except we use `op_tuple_fragment`, which
takes two arguments: the tuple type and the index. If our struct was
instead a tuple, we would have:

```
  debug_value %1 : $Int, var, (name "the_tuple", loc "file.swift":8:7), type $(x: Int, y: Int), expr op_tuple_fragment:$(x: Int, y: Int):1, loc "file.swift":9:4
```

It is worth noting that a SIL DIExpression is similar to
[!DIExpression](https://www.llvm.org/docs/LangRef.html#diexpression) in
LLVM debug info metadata. While LLVM represents `!DIExpression` are a
list of 64-bit integers, SIL DIExpression can have elements with various
types, like AST nodes or strings.

The `[trace]` flag is available for compiler unit testing. It is not
produced during normal compilation. It is used combination with internal
logging and optimization controls to select specific values to trace or
to transform. For example, liveness analysis combines all "traced"
values into a single live range with multiple definitions. This exposes
corner cases that cannot be represented by passing valid SIL through the
pipeline.

### debug_step

```
sil-instruction ::= debug_step

debug_step
```

This instruction is inserted by Onone optimizations as a replacement for
deleted instructions to ensure that it's possible to set a breakpoint
on its location.

It is code-generated to a NOP instruction.

## Testing

### specify_test

```
sil-instruction ::= 'specify_test' string-literal

specify_test "parsing @trace[3] @function[other].block[2].instruction[1]"
```

Exists only for writing FileCheck tests. Specifies a list of test
arguments which should be used in order to run a particular test "in
the context" of the function containing the instruction.

Parsing of these test arguments is done via
`parseTestArgumentsFromSpecification`.

The following types of test arguments are supported:

-   boolean: `true` `false`

-   unsigned integer: 0...ULONG_MAX

-   string

-   value: `%name`

-   function:
    -  `@function` <-- the current function
    -  `@function[uint]` <-- function at index `uint`
    -  `@function[name]` <-- function named `name`

-   block:
    -  `@block` <-- the block containing the specify_test instruction
    -  `@block[+uint]` <-- the block `uint` blocks after the containing block
    -  `@block[-uint]` <-- the block `uint` blocks before the containing block \
    -  `@block[uint]` <-- the block at index `uint`
    -  `@{function}.{block}` <-- the indicated block in the indicated function Example: `@function[foo].block[2]`

-   trace:
    -  `@trace` <-- the first `debug_value [trace]` in the current function
    -  `@trace[uint]` <-- the `debug_value [trace]` at index `uint`

-   value:
    -  `@{instruction}.result` <-- the first result of the instruction
    -  `@{instruction}.result[uint]` <-- the result at index `uint` produced by the instruction
    -  `@{function}.{trace}` <-- the indicated trace in the indicated function Example: `@function[bar].trace`

-   argument:
    -  `@argument` <-- the first argument of the current block
    -  `@argument[uint]` <-- the argument at index `uint` of the current block 
    -  `@{block}.{argument}` <-- the indicated argument in the indicated block 
    -  `@{function}.{argument}` <-- the indicated argument in the entry block of the indicated function

-   instruction:
    -  `@instruction` <-- the instruction after* the specify_test instruction
    -  `@instruction[+uint]` <-- the instruction `uint` instructions after the `specify_test` instruction
    -  `@instruction[-uint]` <-- the instruction `uint` instructions before the `specify_test` instruction
    -  `@instruction[uint]` <-- the instruction at index `uint`
    -  `@{function}.{instruction}` <-- the indicated instruction in the indicated function Example: `@function[baz].instruction[19]`
    -  `@{block}.{instruction}` <-- the indicated instruction in the indicated block Example: `@function[bam].block.instruction`

-   operand:
    -  `@operand` <-- the first operand
    -  `@operand[uint]` <-- the operand at index `uint`
    -  `@{instruction}.{operand}` <-- the indicated operand of the indicated instruction Examples:
        `@block[19].instruction[2].operand[3]`,
        `@function[2].instruction.operand`

Not counting instructions that are deleted when processing functions for tests. The following instructions currently are deleted:
- `specify_test`
- `debug_value [trace]`

## Profiling

### increment_profiler_counter

```
sil-instruction ::= 'increment_profiler_counter' int-literal ',' string-literal ',' 'num_counters' int-literal ',' 'hash' int-literal

increment_profiler_counter 1, "$foo", num_counters 3, hash 0
```

Increments a given profiler counter for a given PGO function name. This
is lowered to the `llvm.instrprof.increment` LLVM intrinsic. This
instruction is emitted when profiling is enabled, and enables features
such as code coverage and profile-guided optimization.

## Accessing Memory

### load

```
sil-instruction ::= 'load' load-ownership-kind? sil-operand
load-ownership-kind ::= 'trivial'
load-ownership-kind ::= 'copy'
load-ownership-kind ::= 'take'

%1 = load %0 : $*T
// %0 must be of a $*T address type for loadable type $T
// %1 will be of type $T
```

Loads the value at address `%0` from memory. `T` must be a loadable
type. This does not affect the reference count, if any, of the loaded
value; the value must be retained explicitly if necessary. It is
undefined behavior to load from uninitialized memory or to load from an
address that points to deallocated storage.

In OSSA the ownership kind specifies how to handle ownership:
-  **trivial**: the loaded value is trivial and no further action must be taken
                than to load the raw bits of the value
-  **copy**: the loaded value is copied and the original value stays in the
             memory location.
-  **take**: the value is _moved_ from the memory location without copying.
             After the `load`, the memory location remains uninitialized.

### store

```
sil-instruction ::= 'store' sil-value 'to' store-ownership-kind? sil-operand
store-ownership-kind ::= '[trivial]'
store-ownership-kind ::= '[init]'
store-ownership-kind ::= '[assign]'

store %0 to [init] %1 : $*T
// $T must be a loadable type
```

Stores the value `%0` to memory at address `%1`. The type of %1 is `*T`
and the type of `%0` is `T`, which must be a loadable type. This will
overwrite the memory at `%1`.

In OSSA the ownership kind specifies how to handle ownership:
-  **trivial**: the stored value is trivial and no further action must be taken
                than to store the raw bits of the value
-  **init**: the memory is assumed to be _not_ initialized. The (non-trivial)
             value is consumed by the instruction an stored to memory.
-  **assign**: the memory is assumed to be initialized. Before storing the new
               value, the existing memory value is destroyed. The new (non-trivial)
               value is consumed by the instruction an stored to memory.

### load_borrow

```
sil-instruction ::= 'load_borrow' sil-value

%1 = load_borrow %0 : $*T
// $T must be a loadable type
```

Loads the value `%1` from the memory location `%0`. The
[load_borrow](#load_borrow) instruction creates a borrowed scope in
which a read-only borrow value `%1` can be used to read the value stored
in `%0`. The end of scope is delimited by an [end_borrow](#end_borrow)
instruction. All [load_borrow](#load_borrow) instructions must be paired
with exactly one [end_borrow](#end_borrow) instruction along any path
through the program. Until [end_borrow](#end_borrow), it is illegal to
invalidate or store to `%0`.

### store_borrow

```
sil-instruction ::= 'store_borrow' sil-value 'to' sil-operand

%2 = store_borrow %0 to %1 : $*T
// $T must be a loadable type
// %1 must be an alloc_stack $T
// %2 is the return address
```

Stores the value `%0` to a stack location `%1`, which must be an
`alloc_stack $T`. The stack location must not be modified by other
instructions than `store_borrow`. All uses of the store_borrow
destination `` `%1 `` should be via the store_borrow return address `%2`
except dealloc_stack. The stored value is alive until the `end_borrow`.
During its lifetime, the stored value must not be modified or destroyed.
The source value `%0` is borrowed (i.e. not copied) and its borrow scope
must outlive the lifetime of the stored value.

Notionally, the outer borrow scope ensures that there's something to be
addressed. The inner borrow scope provides the address to work with.

### begin_borrow

```
sil-instruction ::= 'begin_borrow' '[lexical]'? sil-operand

%1 = begin_borrow %0 : $T
```

Given a value `%0` with [Owned](#owned) or [Guaranteed](#guaranteed)
ownership, produces a new same typed value with
[Guaranteed](#guaranteed) ownership: `%1`. `%1` is guaranteed to have a
lifetime ending use (e.x.: [end_borrow](#end_borrow)) along all paths
that do not end in [Dead End Blocks](#dead-end-blocks). This
[begin_borrow](#begin_borrow) and the lifetime ending uses of `%1` are
considered to be liveness requiring uses of `%0` and as such in the
region in between this borrow and its lifetime ending use, `%0` must be
live. This makes sense semantically since `%1` is modeling a new value
with a dependent lifetime on `%0`.

The optional `lexical` attribute specifies that the operand corresponds
to a local variable with a lexical lifetime in the Swift source, so
special care must be taken when moving the end_borrow. Compare to the
`var_decl` attribute. See [Variable Lifetimes](Ownership.md#variable-lifetimes).

The optional `pointer_escape` attribute specifies that a pointer to the
operand escapes within the borrow scope introduced by this begin_borrow.

The optional `var_decl` attribute specifies that the operand corresponds
to a local variable in the Swift source.

This instruction is only valid in functions in Ownership SSA form.

### end_borrow

```
sil-instruction ::= 'end_borrow' sil-operand

// somewhere earlier
// %1 = begin_borrow %0
end_borrow %1 : $T
```

Ends the scope for which the [Guaranteed](#guaranteed) ownership
possessing SILValue `%1` is borrowed from the SILValue `%0`. Must be
paired with at most 1 borrowing instruction (like
[load_borrow](#load_borrow), [begin_borrow](#begin_borrow)) along any
path through the program. In the region in between the borrow
instruction and the [end_borrow](#end_borrow), the original SILValue can
not be modified. This means that:

1.  If `%0` is an address, `%0` cannot be written to.
2.  If `%0` is a non-trivial value, `%0` cannot be destroyed.

We require that `%1` and `%0` have the same type ignoring
SILValueCategory.

This instruction is only valid in functions in Ownership SSA form.

### borrowed from

```
sil-instruction ::= 'borrowed' sil-operand 'from' '(' (sil-operand (',' sil-operand)*)? ')'

bb1(%1 : @owned $T, %2 : @reborrow $T):
  %3 = borrowed %2 : $T from (%1, %0)
  // %0 is an enclosing value, defined in a block, which dominates bb1
  // %3 has type $T and guaranteed ownership
```

Declares the set of enclosing values for a reborrow or forwarded guaranteed phi argument.
An enclosing value is either a dominating enclosing value (`%0`) or an adjacent
phi-argument in the same block (`%1`). In case of an adjacent phi, all
incoming values of the adjacent phi must be enclosing values for the
corresponding incoming value of the argument in all predecessor
blocks.

The borrowed operand (`%2`) must be a reborrow or forwarded guaranteed phi
argument and is forwarded to the instruction result.

The list of enclosing values (operands after `from`) can be empty if the
borrowed operand stems from a borrow introducer with no enclosing value,
e.g. a `load_borrow`.

Reborrow and forwarded guaranteed phi arguments must not have other users than
borrowed-from instructions.

This instruction is only valid in functions in Ownership SSA form.

### end_lifetime

```
sil-instruction ::= 'end_lifetime' sil-operand

// Consumes %0 without destroying it
end_lifetime %0 : $T

// Consumes the memory location %1 without destroying it
end_lifetime %1 : $*T
```

This instruction signifies the end of it's operand's lifetime to the
ownership verifier. It is inserted by the compiler in instances where it
could be illegal to insert a destroy operation. Example: if the operand
had an `undef` value.

The instruction accepts an object or address type.

If its argument is an address type, it's an
identity projection. This instruction is valid only in OSSA and is
lowered to a no-op when lowering to non-OSSA.

### extend_lifetime

```
sil-instruction ::= 'extend_lifetime' sil-operand

// Indicate that %0's linear lifetime extends to this point
extend_lifetime %0 : $X
```

Indicates that a value's linear lifetime extends to this point.
Inserted by OSSALifetimeCompletion(AvailabilityBoundary) in order to
provide the invariant that a value is either consumed OR has an
`extend_lifetime` user on all paths and furthermore that all
uses are within the boundary defined by that set of instructions (the
consumes and the `extend_lifetime`s).

### assign

```
sil-instruction ::= 'assign' sil-value 'to' sil-operand

assign %0 to %1 : $*T
// $T must be a loadable type
```

Represents an abstract assignment of the value `%0` to memory at address
`%1` without specifying whether it is an initialization or a normal
store. The type of %1 is `*T` and the type of `%0` is `T`, which must be
a loadable type. This will overwrite the memory at `%1` and destroy the
value currently held there.

The purpose of the [assign](#assign) instruction is to simplify the
definitive initialization analysis on loadable variables by removing
what would otherwise appear to be a load and use of the current value.
It is produced by SILGen, which cannot know which assignments are meant
to be initializations. If it is deemed to be an initialization, it can
be replaced with a [store](#store); otherwise, it must be replaced with
a sequence that also correctly destroys the current value.

This instruction is only valid in Raw SIL and is rewritten as
appropriate by the definitive initialization pass.

### assign_by_wrapper

```
sil-instruction ::= 'assign_by_wrapper' sil-operand 'to' mode? sil-operand ',' 'init' sil-operand ',' 'set' sil-operand

mode ::= '[init]' | '[assign]' | '[assign_wrapped_value]'

assign_by_wrapper %0 : $S to %1 : $*T, init %2 : $F, set %3 : $G
// $S can be a value or address type
// $T must be the type of a property wrapper.
// $F must be a function type, taking $S as a single argument (or multiple arguments in case of a tuple) and returning $T
// $G must be a function type, taking $S as a single argument (or multiple arguments in case of a tuple) and without a return value
```

Similar to the [assign](#assign) instruction, but the assignment is done
via a delegate.

Initially the instruction is created with no mode. Once the mode is
decided (by the definitive initialization pass), the instruction is
lowered as follows:

If the mode is `initialization`, the function `%2` is called with `%0`
as argument. The result is stored to `%1`. In case of an address type,
`%1` is simply passed as a first out-argument to `%2`.

The `assign` mode works similar to `initialization`, except that the
destination is "assigned" rather than "initialized". This means that
the existing value in the destination is destroyed before the new value
is stored.

If the mode is `assign_wrapped_value`, the function `%3` is called with
`%0` as argument. As `%3` is a setter (e.g. for the property in the
containing nominal type), the destination address `%1` is not used in
this case.

This instruction is only valid in Raw SIL and is rewritten as
appropriate by the definitive initialization pass.

### mark_uninitialized

```
sil-instruction ::= 'mark_uninitialized' '[' mu_kind ']' sil-operand
mu_kind ::= 'var'
mu_kind ::= 'rootself'
mu_kind ::= 'crossmodulerootself'
mu_kind ::= 'derivedself'
mu_kind ::= 'derivedselfonly'
mu_kind ::= 'delegatingself'
mu_kind ::= 'delegatingselfallocated'

%2 = mark_uninitialized [var] %1 : $*T
// $T must be an address
```

Indicates that a symbolic memory location is uninitialized, and must be
explicitly initialized before it escapes or before the current function
returns. This instruction returns its operands, and all accesses within
the function must be performed against the return value of the
mark_uninitialized instruction.

The kind of mark_uninitialized instruction specifies the type of data
the mark_uninitialized instruction refers to:

-   `var`: designates the start of a normal variable live range

-   `rootself`: designates `self` in a struct, enum, or root class

-   `crossmodulerootself`: same as `rootself`, but in a case where it's not really safe to treat `self` as a root because the original module might add more stored properties. This is only used for Swift 4 compatibility.

-   `derivedself`: designates `self` in a derived (non-root) class

-   `derivedselfonly`: designates `self` in a derived (non-root) class
    whose stored properties have already been initialized

-   `delegatingself`: designates `self` on a struct, enum, or class in a
    delegating constructor (one that calls self.init)

-   `delegatingselfallocated`: designates `self` on a class convenience
    initializer's initializing entry point
    
-   `out`: designates an indirectly returned result.

The purpose of the `mark_uninitialized` instruction is to enable
definitive initialization analysis.

It is produced by SILGen, and is only valid in Raw SIL. It is rewritten
as appropriate by the definitive initialization pass.

### mark_function_escape

```
sil-instruction ::= 'mark_function_escape' sil-operand (',' sil-operand)

mark_function_escape %1 : $*T
```

Indicates that a function definition closes over a symbolic memory
location. This instruction is variadic, and all of its operands must be
addresses.

The purpose of the `mark_function_escape` instruction is to enable
definitive initialization analysis for global variables and instance
variables, which are not represented as box allocations.

It is produced by SILGen, and is only valid in Raw SIL. It is rewritten
as appropriate by the definitive initialization pass.

### mark_uninitialized_behavior

```
init-case ::= sil-value sil-apply-substitution-list? '(' sil-value ')' ':' sil-type
set-case ::= sil-value sil-apply-substitution-list? '(' sil-value ')' ':' sil-type
sil-instruction ::= 'mark_uninitialized_behavior' init-case set-case

mark_uninitialized_behavior %init<Subs>(%storage) : $T -> U,
                            %set<Subs>(%self) : $V -> W
```

Indicates that a logical property is uninitialized at this point and
needs to be initialized by the end of the function and before any escape
point for this instruction. Assignments to the property trigger the
behavior's `init` or `set` logic based on the logical initialization
state of the property.

It is expected that the `init-case` is passed some sort of storage and
the `set` case is passed `self`.

This is only valid in Raw SIL.

### copy_addr

```
sil-instruction ::= 'copy_addr' '[take]'? sil-value
                      'to' '[init]'? sil-operand

copy_addr [take] %0 to [init] %1 : $*T
// %0 and %1 must be of the same $*T address type
```

Loads the value at address `%0` from memory and assigns a copy of it
back into memory at address `%1`. A bare `copy_addr` instruction when
`T` is a non-trivial type:

```
copy_addr %0 to %1 : $*T
```

is equivalent to:

```
%new = load %0 : $*T        // Load the new value from the source
%old = load %1 : $*T        // Load the old value from the destination
strong_retain %new : $T            // Retain the new value
strong_release %old : $T           // Release the old
store %new to %1 : $*T      // Store the new value to the destination
```

except that `copy_addr` may be used even if `%0` is of an address-only
type. The `copy_addr` may be given one or both of the `[take]` or
`[init]` attributes:

-   `[take]` destroys the value at the source address in the course of
    the copy.
-   `[init]` indicates that the destination address is uninitialized.
    Without the attribute, the destination address is treated as already
    initialized, and the existing value will be destroyed before the new
    value is stored.

The three attributed forms thus behave like the following loadable type
operations:

```
// take-assignment
  copy_addr [take] %0 to %1 : $*T
// is equivalent to:
  %new = load %0 : $*T
  %old = load %1 : $*T
  // no retain of %new!
  strong_release %old : $T
  store %new to %1 : $*T

// copy-initialization
  copy_addr %0 to [init] %1 : $*T
// is equivalent to:
  %new = load %0 : $*T
  strong_retain %new : $T
  // no load/release of %old!
  store %new to %1 : $*T

// take-initialization
  copy_addr [take] %0 to [init] %1 : $*T
// is equivalent to:
  %new = load %0 : $*T
  // no retain of %new!
  // no load/release of %old!
  store %new to %1 : $*T
```

If `T` is a trivial type, then `copy_addr` is always equivalent to its
take-initialization form.

It is illegal in non-Raw SIL to apply `copy_addr [init]` to a value that
is move only.

### explicit_copy_addr

```
sil-instruction ::= 'explicit_copy_addr' '[take]'? sil-value
                      'to' '[init]'? sil-operand

explicit_copy_addr [take] %0 to [init] %1 : $*T
// %0 and %1 must be of the same $*T address type
```

This instruction is exactly the same as [copy_addr](#copy_addr) except
that it has special behavior for move only types. Specifically, an
`explicit_copy_addr` is viewed as a `copy_addr` that
is allowed on values that are move only. This is only used by a move
checker after it has emitted an error diagnostic to preserve the general
`copy_addr [init]` ban in Canonical SIL on move only types.

### destroy_addr

```
sil-instruction ::= 'destroy_addr' sil-operand

destroy_addr %0 : $*T
// %0 must be of an address $*T type
```

Destroys the value in memory at address `%0`. If `T` is a non-trivial
type, This is equivalent to:

```
%1 = load %0
strong_release %1
```

except that `destroy_addr` may be used even if `%0` is of an
address-only type. This does not deallocate memory; it only destroys the
pointed-to value, leaving the memory uninitialized.

If `T` is a trivial type, then `destroy_addr` can be safely eliminated.
However, a memory location `%a` must not be accessed after
`destroy_addr %a` (which has not yet been eliminated) regardless of its
type.

### tuple_addr_constructor

```
sil-instruction ::= 'tuple_addr_constructor' sil-tuple-addr-constructor-init sil-operand 'with' sil-tuple-addr-constructor-elements
sil-tuple-addr-constructor-init ::= init|assign
sil-tuple-addr-constructor-elements ::= '(' (sil-operand (',' sil-operand)*)? ')'

// %destAddr has the type $*(Type1, Type2, Type3). Note how we convert all of the types
// to their address form.
%1 = tuple_addr_constructor [init] %destAddr : $*(Type1, Type2, Type3) with (%a : $Type1, %b : $*Type2, %c : $Type3)
```

Creates a new tuple in memory from an exploded list of object and
address values. The SSA values form the leaf elements of the exploded
tuple. So for a simple tuple that only has top level tuple elements,
then the instruction lowers as follows:

```
%1 = tuple_addr_constructor [init] %destAddr : $*(Type1, Type2, Type3) with (%a : $Type1, %b : $*Type2, %c : $Type3)
```
-->

```
%0 = tuple_element_addr %destAddr : $*(Type1, Type2, Type3), 0
store %a to [init] %0 : $*Type1
%1 = tuple_element_addr %destAddr : $*(Type1, Type2, Type3), 1
copy_addr %b to [init] %1 : $*Type2
%2 = tuple_element_addr %destAddr : $*(Type1, Type2, Type3), 2
store %2 to [init] %2 : $*Type3
```

A `tuple_addr_constructor` is lowered similarly with each
`store`/`copy_addr` being changed to their dest assign form.

In contrast, if we have a more complicated form of tuple with
sub-tuples, then we read one element from the list as we process the
tuple recursively from left to right. So for instance we would lower as
follows a more complicated tuple:

```
%1 = tuple_addr_constructor [init] %destAddr : $*((), (Type1, ((), Type2)), Type3) with (%a : $Type1, %b : $*Type2, %c : $Type3)
```
->

```
%0 = tuple_element_addr %destAddr : $*((), (Type1, ((), Type2)), Type3), 1
%1 = tuple_element_addr %0 : $*(Type1, ((), Type2)), 0
store %a to [init] %1 : $*Type1
%2 = tuple_element_addr %0 : $*(Type1, ((), Type2)), 1
%3 = tuple_element_addr %2 : $*((), Type2), 1
copy_addr %b to [init] %3 : $*Type2
%4 = tuple_element_addr %destAddr : $*((), (Type1, ((), Type2)), Type3), 2
store %c to [init] %4 : $*Type3
```

This instruction exists to enable for SILGen to init and assign RValues
into tuples with a single instruction. Since an RValue is a potentially
exploded tuple, we are forced to use our representation here. If SILGen
instead just uses separate address projections and stores when it sees
such an aggregate, diagnostic SIL passes can not tell the difference
semantically in between initializing a tuple in parts or at once:

```
var arg = (Type1(), Type2())

// This looks the same at the SIL level...
arg = (a, b)

// to assigning in pieces even though we have formed a new tuple.
arg.0 = a
arg.1 = a
```

### index_addr

```
sil-instruction ::= 'index_addr' ('[' 'stack_protection' ']')? sil-operand ',' sil-operand

%2 = index_addr %0 : $*T, %1 : $Builtin.Int<n>
// %0 must be of an address type $*T
// %1 must be of a builtin integer type
// %2 will be of type $*T
```

Given an address that references into an array of values, returns the
address of the `%1`-th element relative to `%0`. The address must
reference into a contiguous array. It is undefined to try to reference
offsets within a non-array value, such as fields within a homogeneous
struct or tuple type, or bytes within a value, using `index_addr`.
(`Int8` address types have no special behavior in this regard, unlike
`char*` or `void*` in C.) It is also undefined behavior to index out of
bounds of an array, except to index the "past-the-end" address of the
array.

The `stack_protection` flag indicates that stack protection is done for
the pointer origin.

### tail_addr

```
sil-instruction ::= 'tail_addr' sil-operand ',' sil-operand ',' sil-type

%2 = tail_addr %0 : $*T, %1 : $Builtin.Int<n>, $E
// %0 must be of an address type $*T
// %1 must be of a builtin integer type
// %2 will be of type $*E
```

Given an address of an array of `%1` values, returns the address of an
element which is tail-allocated after the array. This instruction is
equivalent to `index_addr` except that the resulting address is
aligned-up to the tail-element type `$E`.

This instruction is used to project the N-th tail-allocated array from
an object which is created by an `alloc_ref` with multiple `tail_elems`.
The first operand is the address of an element of the (N-1)-th array,
usually the first element. The second operand is the number of elements
until the end of that array. The result is the address of the first
element of the N-th array.

It is undefined behavior if the provided address, count and type do not
match the actual layout of tail-allocated arrays of the underlying
object.

### index_raw_pointer

```
sil-instruction ::= 'index_raw_pointer' sil-operand ',' sil-operand

%2 = index_raw_pointer %0 : $Builtin.RawPointer, %1 : $Builtin.Int<n>
// %0 must be of $Builtin.RawPointer type
// %1 must be of a builtin integer type
// %2 will be of type $Builtin.RawPointer
```

Given a `Builtin.RawPointer` value `%0`, returns a pointer value at the
byte offset `%1` relative to `%0`.

### bind_memory

```
sil-instruction ::= 'bind_memory' sil-operand ',' sil-operand 'to' sil-type

%token = bind_memory %0 : $Builtin.RawPointer, %1 : $Builtin.Word to $T
// %0 must be of $Builtin.RawPointer type
// %1 must be of $Builtin.Word type
// %token is an opaque $Builtin.Word representing the previously bound types
// for this memory region.
```

Binds memory at `Builtin.RawPointer` value `%0` to type `$T` with enough
capacity to hold `%1` values. See SE-0107: UnsafeRawPointer.

Produces a opaque token representing the previous memory state for
memory binding semantics. This abstract state includes the type that the
memory was previously bound to along with the size of the affected
memory region, which can be derived from `%1`. The token cannot, for
example, be used to retrieve a metatype. It only serves a purpose when
used by `rebind_memory`, which has no static type information. The token
dynamically passes type information from the first bind_memory into a
chain of rebind_memory operations.

Example:

```
%_      = bind_memory %0   : $Builtin.RawPointer, %numT : $Builtin.Word to $T // holds type 'T'
%token0 = bind_memory %0   : $Builtin.RawPointer, %numU : $Builtin.Word to $U // holds type 'U'
%token1 = rebind_memory %0 : $Builtin.RawPointer, %token0 : $Builtin.Word  // holds type 'T'
%token2 = rebind_memory %0 : $Builtin.RawPointer, %token1 : $Builtin.Word  // holds type 'U'
```

### rebind_memory

```
sil-instruction ::= 'rebind_memory' sil-operand ' 'to' sil-value

%out_token = rebind_memory %0 : $Builtin.RawPointer to %in_token
// %0 must be of $Builtin.RawPointer type
// %in_token represents a cached set of bound types from a prior memory state.
// %out_token is an opaque $Builtin.Word representing the previously bound
// types for this memory region.
```

This instruction's semantics are identical to `bind_memory`, except
that the types to which memory will be bound, and the extent of the
memory region is unknown at compile time. Instead, the bound-types are
represented by a token that was produced by a prior memory binding
operation. `%in_token` must be the result of `bind_memory` or
`rebind_memory`.

### begin_access

```
sil-instruction ::= 'begin_access' '[' sil-access ']' '[' sil-enforcement ']' '[no_nested_conflict]'? '[builtin]'? sil-operand ':' sil-type
sil-access ::= init
sil-access ::= read
sil-access ::= modify
sil-access ::= deinit
sil-enforcement ::= unknown
sil-enforcement ::= static
sil-enforcement ::= dynamic
sil-enforcement ::= unsafe
sil-enforcement ::= signed
%1 = begin_access [read] [unknown] %0 : $*T
// %0 must be of $*T type.
```

Begins an access to the target memory.

The operand must be a *root address derivation*:

-   a function argument,
-   an `alloc_stack` instruction,
-   a `project_box` instruction,
-   a `global_addr` instruction,
-   a `ref_element_addr` instruction, or
-   another `begin_access` instruction.

It will eventually become a basic structural rule of SIL that no memory
access instructions can be directly applied to the result of one of
these instructions; they can only be applied to the result of a
`begin_access` on them. For now, this rule will be conditional based on
compiler settings and the SIL stage.

An access is ended with a corresponding `end_access`. Accesses must be
uniquely ended on every control flow path which leads to either a
function exit or back to the `begin_access` instruction. The set of
active accesses must be the same on every edge into a basic block.

An `init` access takes uninitialized memory and initializes it. It must
always use `static` enforcement.

An `deinit` access takes initialized memory and leaves it uninitialized.
It must always use `static` enforcement.

`read` and `modify` accesses take initialized memory and leave it
initialized. They may use `unknown` enforcement only in the `raw` SIL
stage.

A `no_nested_conflict` access has no potentially conflicting access
within its scope (on any control flow path between it and its
corresponding `end_access`). Consequently, the access will not need to
be tracked by the runtime for the duration of its scope. This access may
still conflict with an outer access scope; therefore may still require
dynamic enforcement at a single point.

A `signed` access is for pointers that are signed in architectures that
support pointer signing.

A `builtin` access was emitted for a user-controlled Builtin (e.g. the
standard library's KeyPath access). Non-builtin accesses are
auto-generated by the compiler to enforce formal access that derives
from the language. A `builtin` access is always fully enforced
regardless of the compilation mode because it may be used to enforce
access outside of the current module.

### end_access

```
sil-instruction ::= 'end_access' ( '[' 'abort' ']' )? sil-operand
```

Ends an access. The operand must be a `begin_access` instruction.

If the `begin_access` is `init` or `deinit`, the `end_access` may be an
`abort`, indicating that the described transition did not in fact take
place.

### begin_unpaired_access

```
sil-instruction ::= 'begin_unpaired_access' '[' sil-access ']' '[' sil-enforcement ']' '[no_nested_conflict]'? '[builtin]'? sil-operand : sil-type, sil-operand : $*Builtin.UnsafeValueBuffer
sil-access ::= init
sil-access ::= read
sil-access ::= modify
sil-access ::= deinit
sil-enforcement ::= unknown
sil-enforcement ::= static
sil-enforcement ::= dynamic
sil-enforcement ::= unsafe
%2 = begin_unpaired_access [read] [dynamic] %0 : $*T, %1 : $*Builtin.UnsafeValueBuffer
// %0 must be of $*T type.
```

Begins an access to the target memory. This has the same semantics and
obeys all the same constraints as `begin_access`. With the following
exceptions:

-   `begin_unpaired_access` has an additional operand for the scratch
    buffer used to uniquely identify this access within its scope.
-   An access initiated by `begin_unpaired_access` must end with
    `end_unpaired_access` unless it has the `no_nested_conflict` flag. A
    `begin_unpaired_access` with `no_nested_conflict` is effectively an
    instantaneous access with no associated scope.
-   The associated `end_unpaired_access` must use the same scratch
    buffer.

### end_unpaired_access

```
sil-instruction ::= 'end_unpaired_access' ( '[' 'abort' ']' )? '[' sil-enforcement ']' sil-operand : $*Builtin.UnsafeValueBuffer
sil-enforcement ::= unknown
sil-enforcement ::= static
sil-enforcement ::= dynamic
sil-enforcement ::= unsafe
end_unpaired_access [dynamic] %0 : $*Builtin.UnsafeValueBuffer
```

Ends an access. This has the same semantics and constraints as
`end_access` with the following exceptions:

-   The single operand refers to the scratch buffer that uniquely
    identified the access with this scope.
-   The enforcement level is reiterated, since the corresponding
    `begin_unpaired_access` may not be statically discoverable. It must
    be identical to the `begin_unpaired_access` enforcement.

## Reference Counting

These instructions handle reference counting of heap objects. The _retain_ and
_release_ family of instructions are only available in non-OSSA. They are
lowered from OSSA's _copy and _destroy_ operations.

After lowering OSSA, retain and release operations, are never implicit in
SIL and always must be explicitly performed where needed. Retains and
releases on the value may be freely moved, and balancing retains and
releases may be deleted, so long as an owning retain count is maintained
for the uses of the value.

All reference-counting operations are defined to work correctly on null
references (whether strong, unowned, or weak). A non-null reference must
actually refer to a valid object of the indicated type (or a subtype).
Address operands are required to be valid and non-null.

### strong_retain

```
sil-instruction ::= 'strong_retain' sil-operand

strong_retain %0 : $T
// $T must be a reference type
```

Increases the strong retain count of the heap object referenced by `%0`.

This instruction is _not_ available in OSSA.

### strong_release

```
strong_release %0 : $T
// $T must be a reference type.
```

Decrements the strong reference count of the heap object referenced by
`%0`. If the release operation brings the strong reference count of the
object to zero, the object is destroyed and `@weak` references are
cleared. When both its strong and unowned reference counts reach zero,
the object's memory is deallocated.

This instruction is _not_ available in OSSA.

### begin_dealloc_ref

```
%2 = begin_dealloc_ref %0 : $T of %1 : $V
// $T and $V must be reference types where $T is or is derived from $V
// %1 must be an alloc_ref or alloc_ref_dynamic instruction
```

Explicitly sets the state of the object referenced by `%0` to
deallocated. This is the same operation what's done by a strong_release
immediately before it calls the deallocator of the object.

It is expected that the strong reference count of the object is one.
Furthermore, no other thread may increment the strong reference count
during execution of this instruction.

Marks the beginning of a de-virtualized destructor of a class. Returns
the reference operand. Technically, the returned reference is the same
as the operand. But it's important that optimizations see the result as
a different SSA value than the operand. This is important to ensure the
correctness of `ref_element_addr [immutable]` for let-fields, because in
the destructor of a class its let-fields are not immutable anymore.

The first operand `%0` must be physically the same reference as the
second operand `%1`. The second operand has no ownership or code
generation implications and it's purpose is purly to enforce that the
object allocation is present in the same function and trivially visible
from the `begin_dealloc_ref` instruction.

### end_init_let_ref

```
%1 = end_init_let_ref %0 : $T
// $T must be a reference type.
```

Marks the point where all let-fields of a class are initialized.

Returns the reference operand. Technically, the returned reference is
the same as the operand. But it's important that optimizations see the
result as a different SSA value than the operand. This is important to
ensure the correctness of `ref_element_addr [immutable]` for let-fields,
because in the initializer of a class, its let-fields are not immutable,
yet.

### strong_copy_unowned_value

```
sil-instruction ::= 'strong_copy_unowned_value' sil-operand

%1 = strong_copy_unowned_value %0 : $@unowned T
// %1 will be a strong @owned value of type $T.
// $T must be a reference type
```

Asserts that the strong reference count of the heap object referenced by
`%0` is still positive, then increments the reference count and returns
a new strong reference to `%0`. The intention is that this instruction
is used as a "safe ownership conversion" from `unowned` to `strong`.

### strong_retain_unowned

```
sil-instruction ::= 'strong_retain_unowned' sil-operand

strong_retain_unowned %0 : $@unowned T
// $T must be a reference type
```

Asserts that the strong reference count of the heap object referenced by
`%0` is still positive, then increases it by one.

This instruction is _not_ available in OSSA.

### unowned_retain

```
sil-instruction ::= 'unowned_retain' sil-operand

unowned_retain %0 : $@unowned T
// $T must be a reference type
```

Increments the unowned reference count of the heap object underlying
`%0`.

This instruction is _not_ available in OSSA.

### unowned_release

```
sil-instruction ::= 'unowned_release' sil-operand

unowned_release %0 : $@unowned T
// $T must be a reference type
```

Decrements the unowned reference count of the heap object referenced by
`%0`. When both its strong and unowned reference counts reach zero, the
object's memory is deallocated.

This instruction is _not_ available in OSSA.

### load_weak

```
sil-instruction ::= 'load_weak' '[take]'? sil-operand

load_weak [take] %0 : $*@sil_weak Optional<T>
// $T must be an optional wrapping a reference type
```

Increments the strong reference count of the heap object held in the
operand, which must be an initialized weak reference. The result is
value of type `$Optional<T>`, except that it is `null` if the heap
object has begun deallocation.

If `[take]` is specified then the underlying weak reference is
invalidated implying that the weak reference count of the loaded value
is decremented. If `[take]` is not specified then the underlying weak
reference count is not affected by this operation (i.e. it is a +0 weak
ref count operation). In either case, the strong reference count will be
incremented before any changes to the weak reference count.

This operation must be atomic with respect to the final `strong_release`
on the operand heap object. It need not be atomic with respect to
`store_weak`/`weak_copy_value` or `load_weak`/`strong_copy_weak_value`
operations on the same address.

### strong_copy_weak_value

```
sil-instruction ::= 'strong_copy_weak_value' sil-operand

%1 = strong_copy_weak_value %0 : $@sil_weak Optional<T>
// %1 will be a strong @owned value of type $Optional<T>.
// $T must be a reference type
// $@sil_weak Optional<T> must be address-only
```

Only valid in opaque values mode. Lowered by AddressLowering to
load_weak.

If the heap object referenced by `%0` has not begun deallocation,
increments its strong reference count and produces the value
`Optional.some` holding the object. Otherwise, produces the value
`Optional.none`.

This operation must be atomic with respect to the final `strong_release`
on the operand heap object. It need not be atomic with respect to
`store_weak`/`weak_copy_value` or `load_weak`/`strong_copy_weak_value`
operations on the same address.

### store_weak

```
sil-instruction ::= 'store_weak' sil-value 'to' '[init]'? sil-operand

store_weak %0 to [init] %1 : $*@sil_weak Optional<T>
// $T must be an optional wrapping a reference type
```

Initializes or reassigns a weak reference. The operand may be `nil`.

If `[init]` is given, the weak reference must currently either be
uninitialized or destroyed. If it is not given, the weak reference must
currently be initialized. After the evaluation:

-   The value that was originally referenced by the weak reference will
    have its weak reference count decremented by 1.
-   If the optionally typed operand is non-nil, the strong reference
    wrapped in the optional has its weak reference count incremented
    by 1. In contrast, the reference's strong reference count is not
    touched.

This operation must be atomic with respect to the final `strong_release`
on the operand (source) heap object. It need not be atomic with respect
to `store_weak`/`weak_copy_value` or
`load_weak`/`strong_copy_weak_value` operations on the same address.

### weak_copy_value

```
sil-instruction ::= 'weak_copy_value' sil-operand

%1 = weak_copy_value %0 : $Optional<T>
// %1 will be an @owned value of type $@sil_weak Optional<T>.
// $T must be a reference type
// $@sil_weak Optional<T> must be address-only
```

Only valid in opaque values mode. Lowered by AddressLowering to
store_weak.

If `%0` is non-nil, produces the value `@sil_weak Optional.some` holding
the object and increments the weak reference count by 1. Otherwise,
produces the value `Optional.none` wrapped in a `@sil_weak` box.

This operation must be atomic with respect to the final `strong_release`
on the operand (source) heap object. It need not be atomic with respect
to `store_weak`/`weak_copy_value` or
`load_weak`/`strong_copy_weak_value` operations on the same address.

### load_unowned

```
sil-instruction ::= 'load_unowned' '[take]'? sil-operand

%1 = load_unowned [take] %0 : $*@sil_unowned T
// T must be a reference type
```

Increments the strong reference count of the object stored at `%0`.

Decrements the unowned reference count of the object stored at `%0` if
`[take]` is specified. Additionally, the storage is invalidated.

Requires that the strong reference count of the heap object stored at
`%0` is positive. Otherwise, traps.

This operation must be atomic with respect to the final `strong_release`
on the operand (source) heap object. It need not be atomic with respect
to `store_unowned`/`unowned_copy_value` or
`load_unowned`/`strong_copy_unowned_value` operations on the same
address.

### store_unowned

```
sil-instruction ::= 'store_unowned' sil-value 'to' '[init]'? sil-operand

store_unowned %0 to [init] %1 : $*@sil_unowned T
// T must be a reference type
```

Increments the unowned reference count of the object at `%0`.

Decrements the unowned reference count of the object previously stored
at `%1` if `[init]` is not specified.

The storage must be initialized iff `[init]` is not specified.

This operation must be atomic with respect to the final `strong_release`
on the operand (source) heap object. It need not be atomic with respect
to `store_unowned`/`unowned_copy_value` or
`load_unowned`/`strong_copy_unowned_value` operations on the same
address.

### unowned_copy_value

```
sil-instruction ::= 'unowned_copy_value' sil-operand

%1 = unowned_copy_value %0 : $T
// %1 will be an @owned value of type $@sil_unowned T.
// $T must be a reference type
// $@sil_unowned T must be address-only
```

Only valid in opaque values mode. Lowered by AddressLowering to
store_unowned.

Increments the unowned reference count of the object at `%0`.

Wraps the operand in an instance of `@sil_unowned`.

This operation must be atomic with respect to the final `strong_release`
on the operand (source) heap object. It need not be atomic with respect
to `store_unowned`/`unowned_copy_value` or
`load_unowned`/`strong_copy_unowned_value` operations on the same
address.

### fix_lifetime

```
sil-instruction :: 'fix_lifetime' sil-operand

fix_lifetime %0 : $T
// Fix the lifetime of a value %0
fix_lifetime %1 : $*T
// Fix the lifetime of the memory object referenced by %1
```

Acts as a use of a value operand, or of the value in memory referenced
by an address operand. Optimizations may not move operations that would
destroy the value, such as `release_value`, `strong_release`,
`copy_addr [take]`, or `destroy_addr`, past this instruction.

### mark_dependence

```
sil-instruction :: 'mark_dependence' mark-dep-option? sil-operand 'on' sil-operand
mark-dep-option ::= '[nonescaping]'
mark-dep-option ::= '[unresolved]'

%2 = mark_dependence %value : $*T on %base : $Builtin.NativeObject
```

`%base` must not be identical to `%value`.

The value of the result depends on the value of `%base`.
Operations that would destroy `%base` must not be moved before any
instructions that depend on the result of this instruction, exactly as
if the address had been directly derived from that operand (e.g. using
`ref_element_addr`).

The result is the forwarded value of `%value`. If `%value` is an
address, then result is also an address, and the semantics are the
same as the non-address form: the dependency is on any value derived
from the resulting address. The value could also be a
Builtin.RawPointer or a struct containing the same, in which case,
pointed-to values have a dependency if they are derived from this
instruction's result. Note that in-memory values are only dependent on
base if they are derived from this instruction's result. In this
example, the load of `%dependent_value` depends on `%base`, but the
load of `%independent_value` does not:

```
%dependent_address = mark_dependence %original_address on %base
%dependent_value = load [copy] %dependent_address
%independent_value = load %original_address
destroy_value %base
```

`%base` may have either object or address type. If it is an address,
then the dependency is on the current value stored at the address.

The optional `nonescaping` attribute indicates that the lifetime
guarantee is statically verifiable via a def-use walk starting at this
instruction's result. No value derived from a nonescaping
`mark_dependence` may have a bitwise escape (conversion to
UnsafePointer) or pointer escape (unknown use). The `unresolved`
attribute indicates that this verification is required but has not yet
been diagnosed.

`mark_dependence` may only have a non-`Escapable` result if it also
has a `nonescaping` or `unresolved` attribute. A non-`Escapable`
`mark_dependence` extends the lifetime of `%base` through copies of
`%value` and values transitively forwarded from those copies. If
`%value` is an address, then that includes loads from the
address. None of those values may be used by a bitwise escape
(conversion to UnsafePointer) or pointer escape (unknown use). In this
example, the apply depends on `%base` because `%value` has a
non-`Escapable` type:

```
%dependent_address = mark_dependence [nonescaping] %value : %*NonescapableType on %base
%dependent_value = load %dependent_address
%copied_value = copy_value %dependent_value
apply %f(%dependent_value)
destroy_value %base
```

### mark_dependence_addr

```
sil-instruction :: 'mark_dependence_addr' mark-dep-option? sil-operand 'on' sil-operand
mark-dep-option ::= '[nonescaping]'
mark-dep-option ::= '[unresolved]'

mark_dependence_addr [nonescaping] %address : $*T on %base : $Builtin.NativeObject
```

The in-memory value at `%address` depends on the value of `%base`.
Operations that would destroy `%base` must not be moved before any
instructions that depend on that value, exactly as if the location at
`%address` aliases `%base` on all paths reachable from this instruction.

In this example, the load of `%dependent_value` depends on `%base`:

```
mark_dependence_addr %address on %base
%dependent_value = load [copy] %address
destroy_value %base
```

`%base` may have either object or address type. If it is an address,
then the dependency is on the current value stored at the address.

The optional `nonescaping` attribute indicates that the lifetime
guarantee is statically verifiable via a data flow over all paths
reachable from this instruction considering all addresses that may
alias with `%address`. No aliasing address may be used by a bitwise
escape (conversion to UnsafePointer) or pointer escape (unknown
use). The `unresolved` attribute indicates that this verification is
required but has not yet been diagnosed.

`mark_dependence_addr` may only have a non-`Escapable` `%address` if
it also has a `nonescaping` or `unresolved` attribute. A
non-`Escapable` `mark_dependence_addr` extends the lifetime of `%base`
through values loaded from the memory location at `%address` and
through any transitively forwarded or copied values. None of those
values may be used by a bitwise escape (conversion to UnsafePointer)
or pointer escape (unknown use). In this example, the apply depends on
`%base` because `%address` has a non-`Escapable` type:

```
mark_dependence_addr [nonescaping] %address : %*NonescapableType on %base
%dependent_value = load %address
%copied_value = copy_value %dependent_value
apply %f(%dependent_value)
destroy_value %base
```

### is_unique

```
sil-instruction ::= 'is_unique' sil-operand

%1 = is_unique %0 : $*T
// $T must be a reference-counted type
// %1 will be of type Builtin.Int1
```

Checks whether %0 is the address of a unique reference to a memory object.
Returns 1 if the strong reference count is 1, and 0 if the strong reference
count is greater than 1.

A discussion of the semantics can be found in the [ARC Optimization](ARC-Optimization.md) document.

### begin_cow_mutation

```
sil-instruction ::= 'begin_cow_mutation' '[native]'? sil-operand

(%1, %2) = begin_cow_mutation %0 : $C
// $C must be a reference-counted type
// %1 will be of type Builtin.Int1
// %2 will be of type C
```

Checks whether `%0` is a unique reference to a memory object. Returns 1 in
the first result if the strong reference count is 1, and 0 if the strong
reference count is greater than 1.

Returns the reference operand in the second result. The returned
reference can be used to mutate the object. Technically, the returned
reference is the same as the operand. But it's important that
optimizations see the result as a different SSA value than the operand.
This is important to ensure the correctness of
`ref_element_addr [immutable]`.

The operand is consumed and the second result is returned as owned.

The optional `native` attribute specifies that the operand has native
Swift reference counting.

For details see [Copy-on-Write Representation](SIL.md#Copy-on-Write-Representation).

### end_cow_mutation

```
sil-instruction ::= 'end_cow_mutation' '[keep_unique]'? sil-operand

%1 = end_cow_mutation %0 : $C
// $C must be a reference-counted type
// %1 will be of type C
```

Marks the end of the mutation of a reference counted object. Returns the
reference operand. Technically, the returned reference is the same as
the operand. But it's important that optimizations see the result as a
different SSA value than the operand. This is important to ensure the
correctness of `ref_element_addr [immutable]`.

The operand is consumed and the result is returned as owned. The result
is guaranteed to be uniquely referenced.

The optional `keep_unique` attribute indicates that the optimizer must
not replace this reference with a not uniquely reference object.

For details see [Copy-on-Write Representation](SIL.md#Copy-on-Write-Representation).

### end_cow_mutation_addr

```
sil-instruction ::= 'end_cow_mutation_addr' sil-operand

end_cow_mutation_addr %0 : $*T
// %0 must be of an address $*T type
```

This instruction marks the end of mutation of an address. The address could be
an opaque archetype, a struct, tuple or enum type and the end_cow_mutation_addr
will apply to all members contained within it.
It is currently only generated in cases where we maybe deriving a MutableSpan from
`%0` since it is not possible to schedule an `end_cow_mutation` in the standard
library automatically for Array.mutableSpan etc.

### destroy_not_escaped_closure

```
sil-instruction ::= 'destroy_not_escaped_closure' sil-operand

%1 = destroy_not_escaped_closure %0 : $@callee_guaranteed () -> ()
// %0 must be an escaping swift closure.
// %1 will be of type Builtin.Int1
```

Checks if the closure context escaped and then destroys the context.
The escape-check is done by checking if its reference count is exactly 1.
Returns true if it is.

### copy_block

```
sil-instruction :: 'copy_block' sil-operand

%1 = copy_block %0 : $@convention(block) T -> U
```

Performs a copy of an Objective-C block. Unlike retains of other
reference-counted types, this can produce a different value from the
operand if the block is copied from the stack to the heap.

### copy_block_without_escaping

```
sil-instruction :: 'copy_block_without_escaping' sil-operand 'withoutEscaping' sil-operand

%1 = copy_block %0 : $@convention(block) T -> U withoutEscaping %1 : $T -> U
```

Performs a copy of an Objective-C block. Unlike retains of other
reference-counted types, this can produce a different value from the
operand if the block is copied from the stack to the heap.

Additionally, consumes the `withoutEscaping` operand `%1` which is the
closure sentinel. SILGen emits these instructions when it passes
@noescape swift closures to Objective C. A mandatory SIL pass will
lower this instruction into a `copy_block` and a
`is_escaping`/`cond_fail`/`destroy_value` at the end of the lifetime of
the objective c closure parameter to check whether the sentinel closure
was escaped.

## Literals

These instructions bind SIL values to literal constants or to global
entities.

### function_ref

```
sil-instruction ::= 'function_ref' sil-function-name ':' sil-type

%1 = function_ref @function : $@convention(thin) T -> U
// $@convention(thin) T -> U must be a thin function type
// %1 has type $T -> U
```

Creates a reference to a SIL function.

### dynamic_function_ref

```
sil-instruction ::= 'dynamic_function_ref' sil-function-name ':' sil-type

%1 = dynamic_function_ref @function : $@convention(thin) T -> U
// $@convention(thin) T -> U must be a thin function type
// %1 has type $T -> U
```

Creates a reference to a `dynamically_replacable` SIL
function. A `dynamically_replacable` SIL function can be
replaced at runtime.

For the following Swift code:

```
dynamic func test_dynamically_replaceable() {}

func test_dynamic_call() {
  test_dynamically_replaceable()
}
```

We will generate:

```
sil [dynamically_replacable] @test_dynamically_replaceable : $@convention(thin) () -> () {
bb0:
  %0 = tuple ()
  return %0 : $()
}

sil @test_dynamic_call : $@convention(thin) () -> () {
bb0:
  %0 = dynamic_function_ref @test_dynamically_replaceable : $@convention(thin) () -> ()
  %1 = apply %0() : $@convention(thin) () -> ()
  %2 = tuple ()
  return %2 : $()
}
```

### prev_dynamic_function_ref

```
sil-instruction ::= 'prev_dynamic_function_ref' sil-function-name ':' sil-type

%1 = prev_dynamic_function_ref @function : $@convention(thin) T -> U
// $@convention(thin) T -> U must be a thin function type
// %1 has type $T -> U
```

Creates a reference to a previous implementation of a
`dynamic_replacement` SIL function.

For the following Swift code:

```
@_dynamicReplacement(for: test_dynamically_replaceable())
func test_replacement() {
  test_dynamically_replaceable() // calls previous implementation
}
```

We will generate:

```
sil [dynamic_replacement_for "test_dynamically_replaceable"] @test_replacement : $@convention(thin) () -> () {
bb0:
  %0 = prev_dynamic_function_ref @test_replacement : $@convention(thin) () -> ()
  %1 = apply %0() : $@convention(thin) () -> ()
  %2 = tuple ()
  return %2 : $()
}
```

### global_addr

```
sil-instruction ::= 'global_addr' sil-global-name ':' sil-type ('depends_on' sil-operand)?

%1 = global_addr @foo : $*Builtin.Word
%3 = global_addr @globalvar : $*Builtin.Word depends_on %2
// %2 has type $Builtin.SILToken
```

Creates a reference to the address of a global variable which has been
previously initialized by `alloc_global`. It is undefined behavior to
perform this operation on a global variable which has not been
initialized, except the global variable has a static initializer.

Optionally, the dependency to the initialization of the global can be
specified with a dependency token `depends_on <token>`. This is usually
a `builtin "once"` which calls the initializer for the global variable.

### global_value

```
sil-instruction ::= 'global_value' ('[' 'bare' ']')? sil-global-name ':' sil-type

%1 = global_value @v : $T
```

Returns the value of a global variable which has been previously
initialized by `alloc_global`. It is undefined behavior to perform this
operation on a global variable which has not been initialized, except
the global variable has a static initializer.

The `bare` attribute indicates that the object header is not used
throughout the lifetime of the value. This means, no reference counting
operations are performed on the object and its metadata is not used. The
header of bare objects doesn't need to be initialized.

### integer_literal

```
sil-instruction ::= 'integer_literal' sil-type ',' int-literal

%1 = integer_literal $Builtin.Int<n>, 123
// $Builtin.Int<n> must be a builtin integer type
// %1 has type $Builtin.Int<n>
```

Creates an integer literal value. The result will be of type
`Builtin.Int<n>`, which must be a builtin integer type. The literal
value is specified using Swift's integer literal syntax.

### float_literal

```
sil-instruction ::= 'float_literal' sil-type ',' int-literal

%1 = float_literal $Builtin.FP<n>, 0x3F800000
// $Builtin.FP<n> must be a builtin floating-point type
// %1 has type $Builtin.FP<n>
```

Creates a floating-point literal value. The result will be of type
`Builtin.FP<n>`, which must be a builtin floating-point type. The
literal value is specified as the bitwise representation of the floating
point value, using Swift's hexadecimal integer literal syntax.

### string_literal

```
sil-instruction ::= 'string_literal' encoding string-literal
encoding ::= 'utf8'
encoding ::= 'utf16'
encoding ::= 'objc_selector'

%1 = string_literal "asdf"
// %1 has type $Builtin.RawPointer
```

Creates a reference to a string in the global string table. The result
is a pointer to the data. The referenced string is always
null-terminated. The string literal value is specified using Swift's
string literal syntax (though `()` interpolations are not allowed).
When the encoding is `objc_selector`, the string literal produces a
reference to a UTF-8-encoded Objective-C selector in the Objective-C
method name segment.

### base_addr_for_offset

```
sil-instruction ::= 'base_addr_for_offset' sil-type

%1 = base_addr_for_offset $*S
// %1 has type $*S
```

Creates a base address for offset calculations. The result can be used
by address projections, like `struct_element_addr`, which themselves
return the offset of the projected fields. IR generation simply creates
a null pointer for `base_addr_for_offset`.

## Dynamic Dispatch

These instructions perform dynamic lookup of class and generic methods.

The `class_method` and `super_method` instructions must reference Swift
native methods and always use vtable dispatch.

The `objc_method` and `objc_super_method` instructions must reference
Objective-C methods (indicated by the `foreign` marker on a method
reference, as in `#NSObject.description!foreign`).

Note that `objc_msgSend` invocations can only be used as the callee of
an `apply` instruction or `partial_apply` instruction. They cannot be
stored or used as `apply` or `partial_apply` arguments.

### class_method

```
sil-instruction ::= 'class_method' sil-method-attributes?
                      sil-operand ',' sil-decl-ref ':' sil-type

%1 = class_method %0 : $T, #T.method : $@convention(class_method) U -> V
// %0 must be of a class type or class metatype $T
// #T.method must be a reference to a Swift native method of T or
// of one of its superclasses
// %1 will be of type $U -> V
```

Looks up a method based on the dynamic type of a class or class metatype
instance. It is undefined behavior if the class value is null.

If the static type of the class instance is known, or the method is
known to be final, then the instruction is a candidate for
devirtualization optimization. A devirtualization pass can consult the
module's [VTables](SIL.md#vtables) to find the SIL function that implements
the method and promote the instruction to a static
[function_ref](#function_ref).

### objc_method

```
sil-instruction ::= 'objc_method' sil-method-attributes?
                      sil-operand ',' sil-decl-ref ':' sil-type

%1 = objc_method %0 : $T, #T.method!foreign : $@convention(objc_method) U -> V
// %0 must be of a class type or class metatype $T
// #T.method must be a reference to an Objective-C method of T or
// of one of its superclasses
// %1 will be of type $U -> V
```

Performs Objective-C method dispatch using `objc_msgSend()`.

Objective-C method calls are never candidates for de-virtualization.

### super_method

```
sil-instruction ::= 'super_method' sil-method-attributes?
                      sil-operand ',' sil-decl-ref ':' sil-type

%1 = super_method %0 : $T, #Super.method : $@convention(thin) U -> V
// %0 must be of a non-root class type or class metatype $T
// #Super.method must be a reference to a native Swift method of T's
// superclass or of one of its ancestor classes
// %1 will be of type $@convention(thin) U -> V
```

Looks up a method in the superclass of a class or class metatype
instance.

### objc_super_method

```
sil-instruction ::= 'super_method' sil-method-attributes?
                      sil-operand ',' sil-decl-ref ':' sil-type

%1 = super_method %0 : $T, #Super.method!foreign : $@convention(thin) U -> V
// %0 must be of a non-root class type or class metatype $T
// #Super.method!foreign must be a reference to an ObjC method of T's
// superclass or of one of its ancestor classes
// %1 will be of type $@convention(thin) U -> V
```

This instruction performs an Objective-C message send using
`objc_msgSuper()`.

### witness_method

```
sil-instruction ::= 'witness_method' sil-method-attributes?
                      sil-type ',' sil-decl-ref ':' sil-type

%1 = witness_method $T, #Proto.method 
  : $@convention(witness_method) <Self: Proto> U -> V
// $T must be an archetype
// #Proto.method must be a reference to a method of one of the protocol
//   constraints on T
// <Self: Proto> U -> V must be the type of the referenced method,
//   generic on Self
// %1 will be of type $@convention(thin) <Self: Proto> U -> V
```

Looks up the implementation of a protocol method for a generic type
variable constrained by that protocol. The result will be generic on the
`Self` archetype of the original protocol and have the `witness_method`
calling convention. If the referenced protocol is an `@objc` protocol,
the resulting type has the `objc` calling convention.

## Function Application

These instructions call functions or wrap them in partial application or
specialization thunks.

In the following we allow for [apply](#apply),
[begin_apply](#begin_apply), and [try_apply](#try_apply) to have a
callee or caller actor isolation attached to them:

```
sil-actor-isolation        ::= unspecified
                           ::= actor_instance
                           ::= nonisolated
                           ::= nonisolated_unsafe
                           ::= global_actor
                           ::= global_actor_unsafe

sil-actor-isolation-callee ::= [callee_isolation=sil-actor-isolation]
sil-actor-isolation-caller ::= [caller_isolation=sil-actor-isolation]
```

These can be used to write test cases with actor isolation using these
instructions and is not intended to be used in SILGen today.

### apply

```
sil-instruction ::= 'apply' '[nothrow]'? sil-actor-isolation-callee?
                      sil-actor-isolation-caller? sil-value
                      sil-apply-substitution-list?
                      '(' (sil-value (',' sil-value)*)? ')'
                      ':' sil-type

sil-apply-substitution-list ::= '<' sil-substitution
                                    (',' sil-substitution)* '>'
sil-substitution ::= type '=' type

%r = apply %0(%1, %2, ...) : $(A, B, ...) -> R
// Note that the type of the callee '%0' is specified *after* the arguments
// %0 must be of a concrete function type $(A, B, ...) -> R
// %1, %2, etc. must be of the argument types $A, $B, etc.
// %r will be of the return type $R

%r = apply %0<A, B>(%1, %2, ...) : $<T, U>(T, U, ...) -> R
// %0 must be of a polymorphic function type $<T, U>(T, U, ...) -> R
// %1, %2, etc. must be of the argument types after substitution $A, $B, etc.
// %r will be of the substituted return type $R'
```

Transfers control to function `%0`, passing it the given arguments. In
the instruction syntax, the type of the callee is specified after the
argument list; the types of the argument and of the defined value are
derived from the function type of the callee. The input argument tuple
type is destructured, and each element is passed as an individual
argument. The `apply` instruction does no retaining or releasing of its
arguments by itself; the [calling convention](#calling-convention)'s
retain/release policy must be handled by separate explicit `retain` and
`release` instructions. The return value will likewise not be implicitly
retained or released.

The callee value must have function type. That function type may not
have an error result, except the instruction has the `nothrow` attribute
set. The `nothrow` attribute specifies that the callee has an error
result but does not actually throw. For the regular case of calling a
function with error result, use `try_apply`.

NB: If the callee value is of a thick function type, `apply` currently
consumes the callee value at +1 strong retain count.

If the callee is generic, all of its generic parameters must be bound by
the given substitution list. The arguments and return value is given
with these generic substitutions applied.

### begin_apply

```
sil-instruction ::= 'begin_apply' '[nothrow]'? sil-value
                      sil-apply-substitution-list?
                      '(' (sil-value (',' sil-value)*)? ')'
                      ':' sil-type

(%anyAddr, %float, %token) = begin_apply %0() : $@yield_once () -> (@yields @inout %Any, @yields Float)
// %anyAddr : $*Any
// %float : $Float
// %token is a token

(%anyAddr, %float, %token, %allocation) = begin_apply %0() : $@yield_once_2 () -> (@yields @inout %Any, @yields Float)
// %anyAddr : $*Any
// %float : $Float
// %token is a token
// %allocation is a pointer to a token
```

Transfers control to coroutine `%0`, passing it the given arguments. The
rules for the application generally follow the rules for `apply`,
except:

-   the callee value must have be of single-yield coroutine type
    (`yield_once` or `yield_once_2`)
-   control returns to this function not when the coroutine performs a
    `return`, but when it performs a `yield`, and
-   the instruction results are derived from the yields of the coroutine
    instead of its normal results.

The final (in the case of `@yield_once`) or penultimate (in the case of
`@yield_once_2`) result of a `begin_apply` is a "token", a special value which
can only be used as the operand of an `end_apply`, `abort_apply`, or
`end_borrow` instruction. Before this second instruction is executed, the
coroutine is said to be "suspended", and the token represents a reference to its
suspended activation record.

If the coroutine's kind `yield_once_2`, its final result is an address
of a "token", representing the allocation done by the callee
coroutine. It can only be used as the operand of a `dealloc_stack` which
must appear after the coroutine is resumed.

The other results of the instruction correspond to the yields in the
coroutine type. In general, the rules of a yield are similar to the
rules for a parameter, interpreted as if the coroutine caller (the one
executing the `begin_apply`) were being "called" by the `yield`:

-   If a yield has an indirect convention, the corresponding result will
    have an address type; otherwise it has an object type. For example,
    a result corresponding to an `@in Any` yield will have type `$Any`.
-   The convention attributes are the same as the parameter convention
    attributes, interpreted as if the `yield` were the "call" and the
    `begin_apply` marked the entry to the "callee". For example, an
    `@in Any` yield transfers ownership of the `Any` value reference
    from the coroutine to the caller, which must destroy or move the
    value from that position before ending or aborting the coroutine.

A coroutine optionally may produce normal results. These do not have
`@yields` annotation in the result type tuple. :: (%float, %token) =
begin_apply %0() : $@yield_once () -> (@yields Float, Int)

Normal results of a coroutine are produced by the corresponding
`end_apply` instruction.

A `begin_apply` must be uniquely either ended or aborted before exiting
the function or looping to an earlier portion of the function.

When throwing coroutines are supported, there will need to be a
`try_begin_apply` instruction.

### abort_apply

```
sil-instruction ::= 'abort_apply' sil-value

abort_apply %token
```

Aborts the given coroutine activation, which is currently suspended at a
`yield` instruction. Transfers control to the coroutine and takes the
`unwind` path from the `yield`. Control is transferred back when the
coroutine reaches an `unwind` instruction.

The operand must always be the token result of a `begin_apply`
instruction, which is why it need not specify a type.

Throwing coroutines will not require a new instruction for aborting a
coroutine; a coroutine is not allowed to throw when it is being aborted.

### end_apply

```
sil-instruction ::= 'end_apply' sil-value 'as' sil-type

end_apply %token as $()
```

Ends the given coroutine activation, which is currently suspended at a
`yield` instruction. Transfers control to the coroutine and takes the
`resume` path from the `yield`. Control is transferred back when the
coroutine reaches a `return` instruction.

The operand must always be the token result of a `begin_apply`
instruction, which is why it need not specify a type.

The result of `end_apply` is the normal result of the coroutine function
(the operand of the `return` instruction)."

When throwing coroutines are supported, there will need to be a
`try_end_apply` instruction.

### partial_apply

```
sil-instruction ::= 'partial_apply' partial-apply-attr* sil-value
                      sil-apply-substitution-list?
                      '(' (sil-value (',' sil-value)*)? ')'
                      ':' sil-type
partial-apply-attr ::= '[callee_guaranteed]'
partial-apply-attr ::= '[isolated_any]'
partial-apply-attr ::= '[on_stack]'

%c = partial_apply %0(%1, %2, ...) : $(Z..., A, B, ...) -> R
// Note that the type of the callee '%0' is specified *after* the arguments
// %0 must be of a concrete function type $(Z..., A, B, ...) -> R
// %1, %2, etc. must be of the argument types $A, $B, etc.,
//   of the tail part of the argument tuple of %0
// %c will be of the partially-applied thick function type (Z...) -> R

%c = partial_apply %0<A, B>(%1, %2, ...) : $(Z..., T, U, ...) -> R
// %0 must be of a polymorphic function type $<T, U>(T, U, ...) -> R
// %1, %2, etc. must be of the argument types after substitution $A, $B, etc.
//   of the tail part of the argument tuple of %0
// %r will be of the substituted thick function type $(Z'...) -> R'
```

Creates a closure by partially applying the function `%0` to a partial
sequence of its arguments. This instruction is used to implement
closures.

A local function in Swift that captures context, such as `bar` in the
following example:

```
func foo(_ x:Int) -> Int {
  func bar(_ y:Int) -> Int {
    return x + y
  }
  return bar(1)
}
```

lowers to an uncurried entry point and is curried in the enclosing
function:

```
func @bar : $@convention(thin) (Int, @box Int, *Int) -> Int {
entry(%y : $Int, %x_box : $@box Int, %x_address : $*Int):
  // ... body of bar ...
}

func @foo : $@convention(thin) Int -> Int {
entry(%x : $Int):
  // Create a box for the 'x' variable
  %x_box = alloc_box $Int
  %x_addr = project_box %x_box : $@box Int
  store %x to %x_addr : $*Int

  // Create the bar closure
  %bar_uncurried = function_ref @bar : $(Int, Int) -> Int
  %bar = partial_apply %bar_uncurried(%x_box, %x_addr) 
    : $(Int, Builtin.NativeObject, *Int) -> Int

  // Apply it
  %1 = integer_literal $Int, 1
  %ret = apply %bar(%1) : $(Int) -> Int

  // Clean up
  release %bar : $(Int) -> Int
  return %ret : $Int
}
```

**Erased Isolation**: If the `partial_apply` is marked with the flag
`[isolated_any]`, the first applied argument must have type
`Optional<any Actor>`. In addition to being provided as an argument to
the partially-applied function, this value will be stored in a special
place in the context and can be recovered with
`function_extract_isolation`. The result type of the `partial_apply`
will be an `@isolated(any)` function type.

**Ownership Semantics of Closure Context during Invocation**: By
default, an escaping `partial_apply` (`partial_apply` without
`[on_stack]]` creates a closure whose invocation takes ownership of the
context, meaning that a call implicitly releases the closure.

If the `partial_apply` is marked with the flag `[callee_guaranteed]`,
the invocation instead uses a caller-guaranteed model, where the caller
promises not to release the closure while the function is being called.
The result type of the `partial_apply` will be a `@callee_guaranteed`
function type.

**Captured Value Ownership Semantics**: In the instruction syntax, the
type of the callee is specified after the argument list; the types of
the argument and of the defined value are derived from the function type
of the callee. Even so, the ownership requirements of the partial apply
are not the same as that of the callee function (and thus said
signature). Instead:

1.  If the `partial_apply` has a `@noescape` function type
    (`partial_apply [on_stack]`) the closure context is allocated on the
    stack and is initialized to contain the closed-over values without
    taking ownership of those values. The closed-over values are not
    retained and the lifetime of the closed-over values must be managed
    by other instruction independently of the `partial_apply`. The
    lifetime of the stack context of a `partial_apply [on_stack]` must
    be terminated with a `dealloc_stack`.
2.  If the `partial_apply` has an escaping function type (not
    `[on_stack]`) then the closure context will be heap allocated with a
    retain count of 1. Any closed over parameters (except for `@inout`
    parameters) will be consumed by the partial_apply. This ensures that
    no matter when the `partial_apply` is called, the captured arguments
    are alive. When the closure context's reference count reaches zero,
    the contained values are destroyed. If the callee requires an owned
    parameter, then the implicit partial_apply forwarder created by
    IRGen will copy the underlying argument and pass it to the callee.
3.  If an address argument has `@inout_aliasable` convention, the
    closure obtained from `partial_apply` will not own its underlying
    value. The `@inout_aliasable` parameter convention is used when a
    `@noescape` closure captures an `inout` argument.

**Coroutines** `partial_apply` could be used to create closures over
coroutines. Overall, the `partial_apply` of a coroutine is
straightforward: it is another coroutine that captures arguments passed
to the `partial_apply` instruction. This closure applies the original
coroutine (similar to the `begin_apply` instruction) for yields
(suspend) and yields the resulting values. Then it calls the original
coroutine continuation for return or unwind, and forwards the results
(if any) to the caller as well. Currently only the autodiff
transformation produces `partial_apply` for coroutines while
differentiating modify accessors.

**NOTE:** If the callee is generic, all of its generic parameters must
be bound by the given substitution list. The arguments are given with
these generic substitutions applied, and the resulting closure is of
concrete function type with the given substitutions applied. The generic
parameters themselves cannot be partially applied; all of them must be
bound. The result is always a concrete function.

**TODO:** The instruction, when applied to a generic function, currently
implicitly performs abstraction difference transformations enabled by
the given substitutions, such as promoting address-only arguments and
returns to register arguments. This should be fixed.

### builtin

```
sil-instruction ::= 'builtin' string-literal
                      sil-apply-substitution-list?
                      '(' (sil-operand (',' sil-operand)*)? ')'
                      ':' sil-type

%1 = builtin "foo"(%1 : $T, %2 : $U) : $V
// "foo" must name a function in the Builtin module
```

Invokes functionality built into the backend code generator, such as
LLVM-level instructions and intrinsics.

#### Assertion configuration

To be able to support disabling assertions at compile time there is a
builtin `assertion_configuration` . It can
be replaced at compile time by a constant or can stay opaque.

All `assert_configuration` builtins are replaced by the
constant propagation pass to the appropriate constant depending on
compile time settings. Subsequent passes remove dependent unwanted
control flow. Using this mechanism we support conditionally
enabling/disabling of code in SIL libraries depending on the assertion
configuration selected when the library is linked into user code.

There are three assertion configurations: Debug (0), Release (1) and
DisableReplacement (-1).

The optimization flag or a special assert configuration flag determines
the value. Depending on the configuration value, assertions in the
standard library will be executed or not.

The standard library uses this builtin to define an assert that can be
disabled at compile time.

``` none
func assert(...) {
  if Int32(Builtin.assert_configuration() == 0) {
    _assertionFailure(message, ...)
  }
}
```

The `assert_configuration` builtin is serialized when we
build the standard library (we recognize the `-parse-stdlib` option and
don't do the constant replacement but leave the function application to
be serialized to SIL).

The compiler flag that influences the value of the
`assert_configuration` builtin is the optimization flag: at
`-Onone` the builtin will be replaced by `Debug` at higher
optimization levels the builtin will be replaced by `Release`.
Optionally, the value to use for replacement can be specified with the
`-assert-config` flag which overwrites the value selected by the
optimization flag (possible values are `Debug`, `Release`,
`DisableReplacement`).

If `assert_configuration` builtin stays opaque until
IRGen, IRGen will replace the application by the constant representing
Debug mode (0). This happens when building the standard library binary.
The generated SIL will retain the builtin but the generated binary
will contain code with assertions enabled.

## Metatypes

These instructions access metatypes, either statically by type name or
dynamically by introspecting class or generic values.

### metatype

```
sil-instruction ::= 'metatype' sil-type

%1 = metatype $T.Type
// %1 has type $T.Type
```

Creates a reference to the metatype object for type `T`.

### value_metatype

```
sil-instruction ::= 'value_metatype' sil-type ',' sil-operand

%1 = value_metatype $T.Type, %0 : $T
// %0 must be a value or address of type $T
// %1 will be of type $T.Type
```

Obtains a reference to the dynamic metatype of the value `%0`.

### existential_metatype

```
sil-instruction ::= 'existential_metatype' sil-type ',' sil-operand

%1 = existential_metatype $P.Type, %0 : $P
// %0 must be a value of class protocol or protocol composition
//   type $P, or an address of address-only protocol type $*P
// %1 will be a $P.Type value referencing the metatype of the
//   concrete value inside %0
```

Obtains the metatype of the concrete value referenced by the existential
container referenced by `%0`.

### objc_protocol

```
sil-instruction ::= 'objc_protocol' protocol-decl : sil-type

%0 = objc_protocol #ObjCProto : $Protocol
```

**TODO:** Fill this in.

## Aggregate Types

These instructions construct and project elements from structs, tuples,
and class instances.

### retain_value

```
sil-instruction ::= 'retain_value' sil-operand

retain_value %0 : $A
```

Retains a loadable value, which simply retains any references it holds.

For trivial types, this is a no-op. For reference types, this is
equivalent to a `strong_retain`. For `@unowned` types, this is
equivalent to an `unowned_retain`. In each of these cases, those are the
preferred forms.

For aggregate types, especially enums, it is typically both easier and
more efficient to reason about aggregate copies than it is to reason
about copies of the subobjects.

This instruction is _not_ available in OSSA.

### retain_value_addr

```
sil-instruction ::= 'retain_value_addr' sil-operand

retain_value_addr %0 : $*A
```

Retains a loadable value inside given address, which simply retains any
references it holds.

This instruction is _not_ available in OSSA.

### unmanaged_retain_value

```
sil-instruction ::= 'unmanaged_retain_value' sil-value

unmanaged_retain_value %0 : $A
```

This instruction has the same local semantics as `retain_value` but:

-   Is valid in ownership qualified SIL.
-   Is not intended to be statically paired at compile time by the
    compiler.

The intention is that this instruction is used to implement unmanaged
constructs.

This instruction is _not_ available in OSSA.

### strong_copy_unmanaged_value

```
sil-instruction ::= 'strong_copy_unmanaged_value' sil-value

%1 = strong_copy_unmanaged_value %0 : $@sil_unmanaged A
// %1 will be a strong @owned $A.
```

This instruction has the same semantics as `copy_value` except that its
input is a trivial `@sil_unmanaged` type that doesn't require ref
counting. This is intended to be used semantically as a "conversion"
like instruction from `unmanaged` to `strong` and thus should never be
removed by the optimizer. Since the returned value is a strong owned
value, this instruction semantically should be treated as performing a
strong copy of the underlying value as if by the value's type lowering.

### copy_value

```
sil-instruction ::= 'copy_value' sil-operand

%1 = copy_value %0 : $A
```

Performs a copy of a loadable value as if by the value's type lowering
and returns the copy. The returned copy semantically is a value that is
completely independent of the operand. In terms of specific types:

1.  For trivial types, this is equivalent to just propagating through
    the trivial value.
2.  For reference types, this is equivalent to performing a
    `strong_retain` operation and returning the reference.
3.  For `@unowned` types, this is equivalent to performing an
    `unowned_retain` and returning the operand.
4.  For aggregate types, this is equivalent to recursively performing a
    `copy_value` on its components, forming a new aggregate from the
    copied components, and then returning the new aggregate.

In ownership qualified functions, a `copy_value` produces a +1 value
that must be consumed at most once along any path through the program.

It is illegal in non-Raw SIL to `copy_value` a value that
is non-copyable.

### explicit_copy_value

```
sil-instruction ::= 'explicit_copy_value' sil-operand

%1 = explicit_copy_value %0 : $A
```

This is exactly the same instruction semantically as
[copy_value](#copy_value) with the exception that when move only
checking is performed, `explicit_copy_value` is
treated as an explicit copy asked for by the user that should not be
rewritten and should be treated as a non-consuming use.

This is used for two things:

1.  Implementing a copy builtin for no implicit copy types.
2.  To enable the move checker, once it has emitted an error diagnostic,
    to still produce valid Ownership SSA SIL at the end of the
    guaranteed optimization pipeline when we enter the Canonical SIL
    stage.

### move_value

```
sil-instruction ::= 'move_value' '[lexical]'? sil-operand

%1 = move_value %0 : $@_moveOnly A
```

Performs a move of the operand, ending its lifetime. When ownership is
enabled, it always takes in an `@owned T` and produces a
new `@owned T`.

1.  For trivial types, this is equivalent to just propagating through
    the trivial value.
2.  For reference types, this is equivalent to ending the lifetime of
    the operand, beginning a new lifetime for the result and setting the
    result to the value of the operand.
3.  For aggregates, the operation is equivalent to performing a
    move_value on each of its fields recursively.

After ownership is lowered, we leave in the move_value to provide a
place for IRGenSIL to know to store a potentially new variable (in case
the move was associated with a let binding).

NOTE: This instruction is used in an experimental feature called 'move
only values'. A move_value instruction is an instruction that
introduces (or injects) a type `T` into the move only value
space.

The `lexical` attribute specifies that the value corresponds to a local
variable with a lexical lifetime in the Swift source. Compare to the
`var_decl` attribute. See [Variable Lifetimes](Ownership.md#variable-lifetimes).

The optional `pointer_escape` attribute specifies that a pointer to the
operand escapes within the scope introduced by this move_value.

The optional `var_decl` attribute specifies that the operand corresponds
to a local variable in the Swift source.

Note: Although ``move_value`` conceptually forwards an owned value, it also
summarizes lifetime attributes for a whole [forward-extended
lifetime](SIL.md#lifetimes); therefore, it is not formally a forwarding
instruction.

### drop_deinit

```
sil-instruction ::= 'drop_deinit' sil-operand

%1 = drop_deinit %0 : $T
// T must be a move-only type
// %1 is an @owned T
%3 = drop_deinit %2 : $*T
// T must be a move-only type
// %2 has type *T
```

This instruction is a marker for a following destroy instruction to
suppress the call of the move-only type's deinitializer. The
instruction accepts an object or address type. If its argument is an
object type it takes in an `@owned T` and produces a new
`@owned T`. If its argument is an address type, it's an
identity projection.

If the operand is an object type, then this is a pseudo type-cast. It
consumes its operand and produces a new value with the same nominal
struct or enum type, but as if the type had no user-defined
deinitializer. It's only use must be a an instruction that ends the
aggregate lifetime, such as `destroy_value`,
`destructure_struct`, or `switch_enum`. If the
use is a `destroy_value`, then prevents the destroy from
invoking the deinitializer. For example:

```
%1 = drop_deinit %0 : $T
destroy_value %1 : $T    // does not invoke deinit()
```

If the operand and result are addresses, drop_deinit ends the lifetime
of the referenced memory value while keeping the value's fields or enum
cases alive. The deinit of the value is not called. The returned address
can be used to access the value's field, e.g. with struct_element_addr,
or enum cases with switch_enum_addr. After the drop_deinit, it is
illegal to destroy its operand or result address with destroy_addr. For
example:

```
%1 = drop_deinit %0 : $S
%2 = struct_element_addr %1 : $*T, #S.field
destroy_addr %2 : $T
```

The instruction is only valid in ownership SIL.

### release_value

```
sil-instruction ::= 'release_value' sil-operand

release_value %0 : $A
```

Destroys a loadable value, by releasing any retainable pointers within
it.

This is defined to be equivalent to storing the operand into a stack
allocation and using 'destroy_addr' to destroy the object there.

For trivial types, this is a no-op. For reference types, this is
equivalent to a `strong_release`. For `@unowned` types, this is
equivalent to an `unowned_release`. In each of these cases, those are
the preferred forms.

For aggregate types, especially enums, it is typically both easier and
more efficient to reason about aggregate destroys than it is to reason
about destroys of the subobjects.

This instruction is _not_ available in OSSA.

### release_value_addr

```
sil-instruction ::= 'release_value_addr' sil-operand

release_value_addr %0 : $*A
```

Destroys a loadable value inside given address, by releasing any
retainable pointers within it.

This instruction is _not_ available in OSSA.

### unmanaged_release_value

```
sil-instruction ::= 'unmanaged_release_value' sil-value

unmanaged_release_value %0 : $A
```

This instruction has the same local semantics as `release_value` but:

-   Is valid in ownership qualified SIL.
-   Is not intended to be statically paired at compile time by the
    compiler.

The intention is that this instruction is used to implement unmanaged
constructs.

This instruction is _not_ available in OSSA.

### destroy_value

```
sil-instruction ::= 'destroy_value' '[dead_end]'? '[poison]'? sil-operand

destroy_value %0 : $A
```

Destroys a loadable value, by releasing any retainable pointers within
it.

This is defined to be equivalent to storing the operand into a stack
allocation and using 'destroy_addr' to destroy the object there.

For trivial types, this is a no-op. For reference types, this is
equivalent to a `strong_release`. For `@unowned` types, this is
equivalent to an `unowned_release`. In each of these cases, those are
the preferred forms.

For aggregate types, especially enums, it is typically both easier and
more efficient to reason about aggregate destroys than it is to reason
about destroys of the subobjects.

The optional `dead_end` attribute specifies that this instruction was
created during lifetime completion and is eligible for deletion during
OSSA lowering.

### autorelease_value

```
sil-instruction ::= 'autorelease_value' sil-operand

autorelease_value %0 : $A
```

**TODO:** Complete this section.

### function_extract_isolation

```
sil-instruction ::= function_extract_isolation sil-operand
```

Reads the isolation of a `@isolated(any)` function value.  The result is always
a borrowed value of type `$Optional<any Actor>`. It is exactly the value that
was originally used to construct the function with `partial_apply
[isolated_any]`.

### tuple

```
sil-instruction ::= 'tuple' sil-tuple-elements
sil-tuple-elements ::= '(' (sil-operand (',' sil-operand)*)? ')'
sil-tuple-elements ::= sil-type '(' (sil-value (',' sil-value)*)? ')'

%1 = tuple (%a : $A, %b : $B, ...)
// $A, $B, etc. must be loadable non-address types
// %1 will be of the "simple" tuple type $(A, B, ...)

%1 = tuple $(a:A, b:B, ...) (%a, %b, ...)
// (a:A, b:B, ...) must be a loadable tuple type
// %1 will be of the type $(a:A, b:B, ...)
```

Creates a loadable tuple value by aggregating multiple loadable values.

If the destination type is a "simple" tuple type, that is, it has no
keyword argument labels or variadic arguments, then the first notation
can be used, which interleaves the element values and types. If keyword
names or variadic fields are specified, then the second notation must be
used, which spells out the tuple type before the fields.

### tuple_extract

```
sil-instruction ::= 'tuple_extract' sil-operand ',' int-literal

%1 = tuple_extract %0 : $(T...), 123
// %0 must be of a loadable tuple type $(T...)
// %1 will be of the type of the selected element of %0
```

Extracts an element from a loadable tuple value.

### tuple_pack_extract

```
sil-instruction ::= 'tuple_pack_extract' sil-value 'of' sil-operand 'as' sil-type

%value = tuple_pack_extract %index of %tuple : $(repeat each T) as $@pack_element("01234567-89AB-CDEF-0123-000000000000") U
// %index must be of $Builtin.PackIndex type
// %tuple must be of tuple type
// %addr will be the result type specified by the 'as' clause
```

Extracts a value at a dynamic index from a tuple value.

Only valid in opaque values mode. Lowered by AddressLowering to
`tuple_pack_element_addr`. For more details, see that instruction.

### tuple_element_addr

```
sil-instruction ::= 'tuple_element_addr' sil-operand ',' int-literal

%1 = tuple_element_addr %0 : $*(T...), 123
// %0 must of a $*(T...) address-of-tuple type
// %1 will be of address type $*U where U is the type of the 123rd
//   element of T
```

Given the address of a tuple in memory, derives the address of an
element within that value.

### tuple_pack_element_addr

```
sil-instruction ::= 'tuple_pack_element_addr' sil-value 'of' sil-operand 'as' sil-type

%addr = tuple_pack_element_addr %index of %tuple : $*(repeat each T) as $*@pack_element("01234567-89AB-CDEF-0123-000000000000") U
// %index must be of $Builtin.PackIndex type
// %tuple must be of address-of-tuple type
// %addr will be of the result type specified by the 'as' clause
```

Given the address of a tuple in memory, derives the address of a dynamic
element within that value.

The *induced pack type* for the tuple operand is the indirect pack type
corresponding to the types of the tuple elements and tuple element
expansions, exactly as if the labels were removed and the parentheses
were replaced with `Pack{`...`}`. For example,
for the tuple type `(repeat Optional<each T>, Float)`, the
induced pack type is `Pack{repeat Optional<each T>, Float}`.

The pack index operand must be a pack indexing instruction. The result
type (given by the `as` clause) must be structurally
well-typed for the pack index and the induced pack type; see the
structural type matching rules for pack indices.

### destructure_tuple

```
sil-instruction ::= 'destructure_tuple' sil-operand

(%elt1, ..., %eltn) = destructure_tuple %0 : $(Elt1Ty, ..., EltNTy)
// %0 must be a tuple of type $(Elt1Ty, ..., EltNTy)
// %eltN must have the type $EltNTy
```

Given a tuple value, split the value into its constituent elements.

### struct

```
sil-instruction ::= 'struct' sil-type '(' (sil-operand (',' sil-operand)*)? ')'

%1 = struct $S (%a : $A, %b : $B, ...)
// $S must be a loadable struct type
// $A, $B, ... must be the types of the physical 'var' fields of $S in order
// %1 will be of type $S
```

Creates a value of a loadable struct type by aggregating multiple
loadable values.

### struct_extract

```
sil-instruction ::= 'struct_extract' sil-operand ',' sil-decl-ref

%1 = struct_extract %0 : $S, #S.field
// %0 must be of a loadable struct type $S
// #S.field must be a physical 'var' field of $S
// %1 will be of the type of the selected field of %0
```

Extracts a physical field from a loadable struct value.

### struct_element_addr

```
sil-instruction ::= 'struct_element_addr' sil-operand ',' sil-decl-ref

%1 = struct_element_addr %0 : $*S, #S.field
// %0 must be of a struct type $S
// #S.field must be a physical 'var' field of $S
// %1 will be the address of the selected field of %0
```

Given the address of a struct value in memory, derives the address of a
physical field within the value.

### destructure_struct

```
sil-instruction ::= 'destructure_struct' sil-operand

(%elt1, ..., %eltn) = destructure_struct %0 : $S
// %0 must be a struct of type $S
// %eltN must have the same type as the Nth field of $S
```

Given a struct, split the struct into its constituent fields.

### object

```
sil-instruction ::= 'object' sil-type '(' (sil-operand (',' sil-operand)*)? ')'

object $T (%a : $A, %b : $B, ...)
// $T must be a non-generic or bound generic reference type
// The first operands must match the stored properties of T
// Optionally there may be more elements, which are tail-allocated to T
```

Constructs a statically initialized object. This instruction can only
appear as final instruction in a global variable static initializer
list.

### vector

```
sil-instruction ::= 'vector' '(' (sil-operand (',' sil-operand)*)? ')'

vector (%a : $T, %b : $T, ...)
// $T must be a non-generic or bound generic reference type
// All operands must have the same type
```

Constructs a statically initialized vector of elements. This instruction
can only appear as final instruction in a global variable static
initializer list.

### vector_base_addr

```
sil-instruction ::= 'vector_base_addr' sil-operand

%1 = vector_base_addr %0 : $*Builtin.FixedArray<N, Element>
// %0 must have type $*Builtin.FixedArray
// %1 will be of the element type of the Builtin.FixedArray
```

Derives the address of the first element of a vector, i.e. a `Builtin.FixedArray`,
from the address of the vector itself.
Addresses of other vector elements can then be derived with `index_addr`.

### ref_element_addr

```
sil-instruction ::= 'ref_element_addr' '[immutable]'? sil-operand ',' sil-decl-ref

%1 = ref_element_addr %0 : $C, #C.field
// %0 must be a value of class type $C
// #C.field must be a non-static physical field of $C
// %1 will be of type $*U where U is the type of the selected field
//   of C
```

Given an instance of a class, derives the address of a physical instance
variable inside the instance. It is undefined behavior if the class
value is null.

The `immutable` attribute specifies that all loads of the same instance
variable from the same class reference operand are guaranteed to yield
the same value. The `immutable` attribute is used to reference COW
buffer elements after an `end_cow_mutation` and before a
`begin_cow_mutation`. The attribute is also used for let-fields of a
class after an `end_init_let_ref` and before a `begin_dealloc_ref`.

### ref_tail_addr

```
sil-instruction ::= 'ref_tail_addr' '[immutable]'? sil-operand ',' sil-type

%1 = ref_tail_addr %0 : $C, $E
// %0 must be a value of class type $C with tail-allocated elements $E
// %1 will be of type $*E
```

Given an instance of a class, which is created with tail-allocated
array(s), derives the address of the first element of the first
tail-allocated array. This instruction is used to project the first
tail-allocated element from an object which is created by an `alloc_ref`
with `tail_elems`. It is undefined behavior if the class instance does
not have tail-allocated arrays or if the element-types do not match.

The `immutable` attribute specifies that all loads of the same instance
variable from the same class reference operand are guaranteed to yield
the same value.

## Enums

These instructions construct and manipulate values of enum type.
Loadable enum values are created with the [enum](#enum) instruction.
Address-only enums require two-step initialization. First, if the case
requires data, that data is stored into the enum at the address
projected by [init_enum_data_addr](#init_enum_data_addr). This step is
skipped for cases without data. Finally, the tag for the enum is
injected with an [inject_enum_addr](#inject_enum_addr) instruction:

```
enum AddressOnlyEnum {
  case HasData(AddressOnlyType)
  case NoData
}

sil @init_with_data : $(AddressOnlyType) -> AddressOnlyEnum {
entry(%0 : $*AddressOnlyEnum, %1 : $*AddressOnlyType):
  // Store the data argument for the case.
  %2 = init_enum_data_addr %0 : $*AddressOnlyEnum, #AddressOnlyEnum.HasData!enumelt
  copy_addr [take] %1 to [init] %2 : $*AddressOnlyType
  // Inject the tag.
  inject_enum_addr %0 : $*AddressOnlyEnum, #AddressOnlyEnum.HasData!enumelt
  return
}

sil @init_without_data : $() -> AddressOnlyEnum {
  // No data. We only need to inject the tag.
  inject_enum_addr %0 : $*AddressOnlyEnum, #AddressOnlyEnum.NoData!enumelt
  return
}
```

Accessing the value of a loadable enum is inseparable from dispatching
on its discriminator and is done with the [switch_enum](#switch_enum)
terminator:

```
enum Foo { case A(Int), B(String) }

sil @switch_foo : $(Foo) -> () {
entry(%foo : $Foo):
  switch_enum %foo : $Foo, case #Foo.A!enumelt: a_dest, case #Foo.B!enumelt: b_dest

a_dest(%a : $Int):
  /* use %a */

b_dest(%b : $String):
  /* use %b */
}
```

An address-only enum can be tested by branching on it using the
[switch_enum_addr](#switch_enum_addr) terminator. Its value can then be
taken by destructively projecting the enum value with
[unchecked_take_enum_data_addr](#unchecked_take_enum_data_addr):

```
enum Foo<T> { case A(T), B(String) }

sil @switch_foo : $<T> (Foo<T>) -> () {
entry(%foo : $*Foo<T>):
  switch_enum_addr %foo : $*Foo<T>, case #Foo.A!enumelt: a_dest, 
    case #Foo.B!enumelt: b_dest

a_dest:
  %a = unchecked_take_enum_data_addr %foo : $*Foo<T>, #Foo.A!enumelt
  /* use %a */

b_dest:
  %b = unchecked_take_enum_data_addr %foo : $*Foo<T>, #Foo.B!enumelt
  /* use %b */
}
```

Both [switch_enum](#switch_enum) and
[switch_enum_addr](#switch_enum_addr) must include a `default` case
unless the enum can be exhaustively switched in the current function,
i.e. when the compiler can be sure that it knows all possible present
and future values of the enum in question. This is generally true for
enums defined in Swift, but there are two exceptions: *non-frozen enums*
declared in libraries compiled with the `-enable-library-evolution`
flag, which may grow new cases in the future in an ABI-compatible way;
and enums marked with the `objc` attribute, for which other bit patterns
are permitted for compatibility with C. All enums imported from C are
treated as "non-exhaustive" for the same reason, regardless of the
presence or value of the `enum_extensibility` Clang attribute.

(See
[SE-0192](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0192-non-exhaustive-enums.md)
for more information about non-frozen enums.)

### enum

```
sil-instruction ::= 'enum' sil-type ',' sil-decl-ref (',' sil-operand)?

%1 = enum $U, #U.EmptyCase!enumelt
%1 = enum $U, #U.DataCase!enumelt, %0 : $T
// $U must be an enum type
// #U.DataCase or #U.EmptyCase must be a case of enum $U
// If #U.Case has a data type $T, %0 must be a value of type $T
// If #U.Case has no data type, the operand must be omitted
// %1 will be of type $U
```

Creates a loadable enum value in the given `case`. If the `case` has a
data type, the enum value will contain the operand value.

### unchecked_enum_data

```
sil-instruction ::= 'unchecked_enum_data' sil-operand ',' sil-decl-ref

%1 = unchecked_enum_data %0 : $U, #U.DataCase!enumelt
// $U must be an enum type
// #U.DataCase must be a case of enum $U with data
// %1 will be of object type $T for the data type of case U.DataCase
```

Unsafely extracts the payload data for an enum `case` from an enum
value. It is undefined behavior if the enum does not contain a value of
the given case.

### init_enum_data_addr

```
sil-instruction ::= 'init_enum_data_addr' sil-operand ',' sil-decl-ref

%1 = init_enum_data_addr %0 : $*U, #U.DataCase!enumelt
// $U must be an enum type
// #U.DataCase must be a case of enum $U with data
// %1 will be of address type $*T for the data type of case U.DataCase
```

Projects the address of the data for an enum `case` inside an enum. This
does not modify the enum or check its value. It is intended to be used
as part of the initialization sequence for an address-only enum. Storing
to the `init_enum_data_addr` for a case followed by `inject_enum_addr`
with that same case is guaranteed to result in a fully-initialized enum
value of that case being stored. Loading from the `init_enum_data_addr`
of an initialized enum value or injecting a mismatched case tag is
undefined behavior.

The address is invalidated as soon as the operand enum is fully
initialized by an `inject_enum_addr`.

### inject_enum_addr

```
sil-instruction ::= 'inject_enum_addr' sil-operand ',' sil-decl-ref

inject_enum_addr %0 : $*U, #U.Case!enumelt
// $U must be an enum type
// #U.Case must be a case of enum $U
// %0 will be overlaid with the tag for #U.Case
```

Initializes the enum value referenced by the given address by overlaying
the tag for the given case. If the case has no data, this instruction is
sufficient to initialize the enum value. If the case has data, the data
must be stored into the enum at the `init_enum_data_addr` address for
the case *before* `inject_enum_addr` is applied. It is undefined
behavior if `inject_enum_addr` is applied for a case with data to an
uninitialized enum, or if `inject_enum_addr` is applied for a case with
data when data for a mismatched case has been stored to the enum.

### unchecked_take_enum_data_addr

```
sil-instruction ::= 'unchecked_take_enum_data_addr' sil-operand ',' sil-decl-ref

%1 = unchecked_take_enum_data_addr %0 : $*U, #U.DataCase!enumelt
// $U must be an enum type
// #U.DataCase must be a case of enum $U with data
// %1 will be of address type $*T for the data type of case U.DataCase
```

Takes the address of the payload for the given enum `case` in-place in
memory. It is undefined behavior if the referenced enum does not contain
a value of the given `case`.

The result shares memory with the original enum value. If an enum
declaration is unconditionally loadable (meaning it's loadable
regardless of any generic parameters), and it has more than one case
with an associated value, then it may embed the enum tag within the
payload area. If this is the case, then
`unchecked_take_enum_data_addr` will clear the tag from the
payload, invalidating the referenced enum value, but leaving the payload
value referenced by the result address valid. In these cases, the enum
memory cannot be reinitialized as an enum until the payload has also
been invalidated.

If an enum has no more than one payload case, or if the declaration is
ever address-only, then `unchecked_take_enum_data_addr` is
guaranteed to be nondestructive, and the payload address can be accessed
without invalidating the enum in these cases. The payload can be
invalidated to invalidate the enum (assuming the enum does not have a
`deinit` at the type level).

### select_enum

```
sil-instruction ::= 'select_enum' sil-operand sil-select-case*
                    (',' 'default' sil-value)?
                    ':' sil-type

%n = select_enum %0 : $U,      
  case #U.Case1!enumelt: %1,           
  case #U.Case2!enumelt: %2, /* ... */ 
  default %3 : $T

// $U must be an enum type
// #U.Case1, Case2, etc. must be cases of enum $U
// %1, %2, %3, etc. must have type $T
// %n has type $T
```

Selects one of the "case" or "default" operands based on the case of
an enum value. This is equivalent to a trivial
[switch_enum](#switch_enum) branch sequence:

```
entry:
  switch_enum %0 : $U,            
    case #U.Case1!enumelt: bb1,           
    case #U.Case2!enumelt: bb2, /* ... */ 
    default bb_default
bb1:
  br cont(%1 : $T) // value for #U.Case1
bb2:
  br cont(%2 : $T) // value for #U.Case2
bb_default:
  br cont(%3 : $T) // value for default
cont(%n : $T):
  // use argument %n
```

but turns the control flow dependency into a data flow dependency. For
address-only enums, [select_enum_addr](#select_enum_addr) offers the
same functionality for an indirectly referenced enum value in memory.

Like [switch_enum](#switch_enum), `select_enum` must have
a `default` case unless the enum can be exhaustively switched in the
current function.

### select_enum_addr

```
sil-instruction ::= 'select_enum_addr' sil-operand sil-select-case*
                    (',' 'default' sil-value)?
                    ':' sil-type

%n = select_enum_addr %0 : $*U,      
  case #U.Case1!enumelt: %1,           
  case #U.Case2!enumelt: %2, /* ... */ 
  default %3 : $T

// %0 must be the address of an enum type $*U
// #U.Case1, Case2, etc. must be cases of enum $U
// %1, %2, %3, etc. must have type $T
// %n has type $T
```

Selects one of the "case" or "default" operands based on the case of
the referenced enum value. This is the address-only counterpart to
[select_enum](#select_enum).

Like [switch_enum_addr](#switch_enum_addr),
`select_enum_addr` must have a `default` case unless
the enum can be exhaustively switched in the current function.

## Protocol and Protocol Composition Types

These instructions create and manipulate values of protocol and protocol
composition type. From SIL's perspective, protocol and protocol
composition types consist of an *existential container*, which is a
generic container for a value of unknown runtime type, referred to as an
"existential type" in type theory. The existential container consists
of a reference to the *witness table(s)* for the protocol(s) referred to
by the protocol type and a reference to the underlying *concrete value*,
which may be either stored in-line inside the existential container for
small values or allocated separately into a buffer owned and managed by
the existential container for larger values.

Depending on the constraints applied to an existential type, an
existential container may use one of several representations:

-   **Opaque existential containers**: If none of the protocols in a
    protocol type are class protocols, then the existential container
    for that type is address-only and referred to in the implementation
    as an *opaque existential container*. The value semantics of the
    existential container propagate to the contained concrete value.
    Applying [copy_addr](#copy_addr) to an opaque existential container
    copies the contained concrete value, deallocating or reallocating
    the destination container's owned buffer if necessary. Applying
    [destroy_addr](#destroy_addr) to an opaque existential container
    destroys the concrete value and deallocates any buffers owned by the
    existential container. The following instructions manipulate opaque
    existential containers:
    -   [init_existential_addr](#init_existential_addr)
    -   [open_existential_addr](#open_existential_addr)
    -   [deinit_existential_addr](#deinit_existential_addr)
-   **Opaque existential containers loadable types**: In the SIL Opaque
    Values mode of operation, we take an opaque value as-is. Said value
    might be replaced with one of the _addr instructions above before
    IR generation. The following instructions manipulate "loadable"
    opaque existential containers:
    -   [init_existential_value](#init_existential_value)
    -   [open_existential_value](#open_existential_value)
    -   [deinit_existential_value](#deinit_existential_value)
-   **Class existential containers**: If a protocol type is constrained
    by one or more class protocols, then the existential container for
    that type is loadable and referred to in the implementation as a
    *class existential container*. Class existential containers have
    reference semantics and can be `retain`-ed and `release`-d. The
    following instructions manipulate class existential containers:
    -   [init_existential_ref](#init_existential_ref)
    -   [open_existential_ref](#open_existential_ref)
-   **Metatype existential containers**: Existential metatypes use a
    container consisting of the type metadata for the conforming type
    along with the protocol conformances. Metatype existential
    containers are trivial types. The following instructions manipulate
    metatype existential containers:
    -   [init_existential_metatype](#init_existential_metatype)
    -   [open_existential_metatype](#open_existential_metatype)
-   **Boxed existential containers**: The standard library `Error`
    protocol uses a size-optimized reference-counted container, which
    indirectly stores the conforming value. Boxed existential containers
    can be `retain`-ed and `release`-d. The following instructions
    manipulate boxed existential containers:
    -   [alloc_existential_box](#alloc_existential_box)
    -   [project_existential_box](#project_existential_box)
    -   [open_existential_box](#open_existential_box)
    -   [open_existential_box_value](#open_existential_box_value)
    -   [dealloc_existential_box](#dealloc_existential_box)

Some existential types may additionally support specialized
representations when they contain certain known concrete types. For
example, when Objective-C interop is available, the `Error` protocol
existential supports a class existential container representation for
`NSError` objects, so it can be initialized from one using
[init_existential_ref](#init_existential_ref) instead of the more
expensive [alloc_existential_box](#alloc_existential_box):

```
bb(%nserror: $NSError):
  // The slow general way to form an Error, allocating a box and
  // storing to its value buffer:
  %error1 = alloc_existential_box $Error, $NSError
  %addr = project_existential_box $NSError in %error1 : $Error
  strong_retain %nserror: $NSError
  store %nserror to %addr : $NSError

  // The fast path supported for NSError:
  strong_retain %nserror: $NSError
  %error2 = init_existential_ref %nserror: $NSError, $Error
```

### init_existential_addr

```
sil-instruction ::= 'init_existential_addr' sil-operand ',' sil-type

%1 = init_existential_addr %0 : $*P, $T
// %0 must be of a $*P address type for non-class protocol or protocol
//   composition type P
// $T must be an AST type that fulfills protocol(s) P
// %1 will be of type $*T', where T' is the maximally abstract lowering
//    of type T
```

Partially initializes the memory referenced by `%0` with an existential
container prepared to contain a value of type `$T`. The result of the
instruction is an address referencing the storage for the contained
value, which remains uninitialized. The contained value must be
`store`-d or `copy_addr`-ed to in order for the existential value to be
fully initialized. If the existential container needs to be destroyed
while the contained value is uninitialized,
[deinit_existential_addr](#deinit_existential_addr) must be used to do
so. A fully initialized existential container can be destroyed with
[destroy_addr](#destroy_addr) as usual. It is undefined behavior to
[destroy_addr](#destroy_addr) a partially-initialized existential
container.

### init_existential_value

```
sil-instruction ::= 'init_existential_value' sil-operand ',' sil-type ','
                                             sil-type

%1 = init_existential_value %0 : $L, $C, $P
// %0 must be of loadable type $L, lowered from AST type $C, conforming to
//    protocol(s) $P
// %1 will be of type $P
```

Loadable version of the above: Inits-up the existential container
prepared to contain a value of type `$P`.

### deinit_existential_addr

```
sil-instruction ::= 'deinit_existential_addr' sil-operand

deinit_existential_addr %0 : $*P
// %0 must be of a $*P address type for non-class protocol or protocol
// composition type P
```

Undoes the partial initialization performed by
[init_existential_addr](#init_existential_addr).
[deinit_existential_addr](#deinit_existential_addr) is only valid for
existential containers that have been partially initialized by
[init_existential_addr](#init_existential_addr) but haven't had their
contained value initialized. A fully initialized existential must be
destroyed with [destroy_addr](#destroy_addr).

### deinit_existential_value

```
sil-instruction ::= 'deinit_existential_value' sil-operand

deinit_existential_value %0 : $P
// %0 must be of a $P opaque type for non-class protocol or protocol
// composition type P
```

Undoes the partial initialization performed by
[init_existential_value](#init_existential_value).
[deinit_existential_value](#deinit_existential_value) is only valid for
existential containers that have been partially initialized by
[init_existential_value](#init_existential_value) but haven't had their
contained value initialized. A fully initialized existential must be
destroyed with [destroy_value](#destroy_value).

### open_existential_addr

```
sil-instruction ::= 'open_existential_addr' sil-allowed-access sil-operand 'to' sil-type
sil-allowed-access ::= 'immutable_access'
sil-allowed-access ::= 'mutable_access'

%1 = open_existential_addr immutable_access %0 : $*P to $*@opened P
// %0 must be of a $*P type for non-class protocol or protocol composition
//   type P
// $*@opened P must be a unique archetype that refers to an opened
// existential type P.
// %1 will be of type $*@opened P
```

Obtains the address of the concrete value inside the existential
container referenced by `%0`. The protocol conformances associated with
this existential container are associated directly with the archetype
`$*@opened P`. This pointer can be used with any operation on
archetypes, such as `witness_method` assuming this operation obeys the
access constraint: The returned address can either allow
`mutable_access` or `immutable_access`. Users of the returned address
may only consume (e.g `destroy_addr` or `copy_addr [take]`) or mutate
the value at the address if they have `mutable_access`.

### open_existential_value

```
sil-instruction ::= 'open_existential_value' sil-operand 'to' sil-type

%1 = open_existential_value %0 : $P to $@opened P
// %0 must be of a $P type for non-class protocol or protocol composition
//   type P
// $@opened P must be a unique archetype that refers to an opened
// existential type P.
// %1 will be of type $@opened P
```

Loadable version of the above: Opens-up the existential container
associated with `%0`. The protocol conformances associated with this
existential container are associated directly with the archetype
`$@opened P`.

### init_existential_ref

```
sil-instruction ::= 'init_existential_ref' sil-operand ':' sil-type ','
                                           sil-type

%1 = init_existential_ref %0 : $C' : $C, $P
// %0 must be of class type $C', lowered from AST type $C, conforming to
//    protocol(s) $P
// $P must be a class protocol or protocol composition type
// %1 will be of type $P
```

Creates a class existential container of type `$P` containing a
reference to the class instance `%0`.

### open_existential_ref

```
sil-instruction ::= 'open_existential_ref' sil-operand 'to' sil-type

%1 = open_existential_ref %0 : $P to $@opened P
// %0 must be of a $P type for a class protocol or protocol composition
// $@opened P must be a unique archetype that refers to an opened
//   existential type P
// %1 will be of type $@opened P
```

Extracts the class instance reference from a class existential
container. The protocol conformances associated with this existential
container are associated directly with the archetype `@opened P`. This
pointer can be used with any operation on archetypes, such as
[witness_method](#witness_method). When the operand is of metatype type,
the result will be the metatype of the opened archetype.

### init_existential_metatype

```
sil-instruction ::= 'init_existential_metatype' sil-operand ',' sil-type

%1 = init_existential_metatype $0 : $@<rep> T.Type, $@<rep> P.Type
// %0 must be of a metatype type $@<rep> T.Type where T: P
// %@<rep> P.Type must be the existential metatype of a protocol or protocol
//    composition, with the same metatype representation <rep>
// %1 will be of type $@<rep> P.Type
```

Creates a metatype existential container of type `$P.Type` containing
the conforming metatype of `$T`.

### open_existential_metatype

```
sil-instruction ::= 'open_existential_metatype' sil-operand 'to' sil-type

%1 = open_existential_metatype %0 : $@<rep> P.Type to $@<rep> (@opened P).Type
// %0 must be of a $P.Type existential metatype for a protocol or protocol
//    composition
// $@<rep> (@opened P).Type must be the metatype of a unique archetype that
//   refers to an opened existential type P, with the same metatype
//   representation <rep>
// %1 will be of type $@<rep> (@opened P).Type
```

Extracts the metatype from an existential metatype. The protocol
conformances associated with this existential container are associated
directly with the archetype `@opened P`.

### alloc_existential_box

```
sil-instruction ::= 'alloc_existential_box' sil-type ',' sil-type

%1 = alloc_existential_box $P, $T
// $P must be a protocol or protocol composition type with boxed
//   representation
// $T must be an AST type that conforms to P
// %1 will be of type $P
```

Allocates a boxed existential container of type `$P` with space to hold
a value of type `$T'`. The box is not fully initialized until a valid
value has been stored into the box. If the box must be deallocated
before it is fully initialized,
[dealloc_existential_box](#dealloc_existential_box) must be used. A
fully initialized box can be `retain`-ed and `release`-d like any
reference-counted type. The
[project_existential_box](#project_existential_box) instruction is used
to retrieve the address of the value inside the container.

### project_existential_box

```
sil-instruction ::= 'project_existential_box' sil-type 'in' sil-operand

%1 = project_existential_box $T in %0 : $P
// %0 must be a value of boxed protocol or protocol composition type $P
// $T must be the most abstracted lowering of the AST type for which the box
// was allocated
// %1 will be of type $*T
```

Projects the address of the value inside a boxed existential container.
The address is dependent on the lifetime of the owner reference `%0`. It
is undefined behavior if the concrete type `$T` is not the same type for
which the box was allocated with
[alloc_existential_box](#alloc_existential_box).

### open_existential_box

```
sil-instruction ::= 'open_existential_box' sil-operand 'to' sil-type

%1 = open_existential_box %0 : $P to $*@opened P
// %0 must be a value of boxed protocol or protocol composition type $P
// %@opened P must be the address type of a unique archetype that refers to
///   an opened existential type P
// %1 will be of type $*@opened P
```

Projects the address of the value inside a boxed existential container,
and uses the enclosed type and protocol conformance metadata to bind the
opened archetype `$@opened P`. The result address is dependent on both
the owning box and the enclosing function; in order to "open" a boxed
existential that has directly adopted a class reference, temporary
scratch space may need to have been allocated.

### open_existential_box_value

```
sil-instruction ::= 'open_existential_box_value' sil-operand 'to' sil-type

%1 = open_existential_box_value %0 : $P to $@opened P
// %0 must be a value of boxed protocol or protocol composition type $P
// %@opened P must be a unique archetype that refers to an opened
//   existential type P
// %1 will be of type $@opened P
```

Projects the value inside a boxed existential container, and uses the
enclosed type and protocol conformance metadata to bind the opened
archetype `$@opened P`.

### dealloc_existential_box

```
sil-instruction ::= 'dealloc_existential_box' sil-operand, sil-type

dealloc_existential_box %0 : $P, $T
// %0 must be an uninitialized box of boxed existential container type $P
// $T must be the AST type for which the box was allocated
```

Deallocates a boxed existential container. The value inside the
existential buffer is not destroyed; either the box must be
uninitialized, or the value must have been projected out and destroyed
beforehand. It is undefined behavior if the concrete type `$T` is not
the same type for which the box was allocated with
[alloc_existential_box](#alloc_existential_box).

## Blocks

Blocks are used in ObjectiveC and are similar to closures.

### project_block_storage

```
sil-instruction ::= 'project_block_storage' sil-operand ':' sil-type
```

### init_block_storage_header

**TODO:** Fill this in. The printing of this instruction looks incomplete
on trunk currently.

## Pack Indexing

These instructions are collectively called the *pack indexing
instructions*. Each of them produces a single value of type
`Builtin.PackIndex`. Instructions that consume pack indices generally
provide a projected element type which is required to be structurally
well-typed for the given pack index and the actual pack type they index
into. This rule depends on the exact pack indexing instruction used and
is described in a section above.

All pack indexing instructions carry an **indexed pack type**, which is
a formal type that must be a pack type. Pack indexing instructions can
be used to index into any pack with the same shape as the indexed pack
type. The components of the actual indexed pack do not need to be
exactly the same as the components of the indexing instruction's
indexed pack type as long as they contain expansions in the same places
and those expansions expand pack parameters with the same shape.

### scalar_pack_index

```
sil-instruction ::= 'scalar_pack_index' int-literal 'of' sil-type

%index = scalar_pack_index 0 of $Pack{Int, repeat each T, Int}
```

Produce the dynamic pack index of a scalar (non-pack-expansion)
component of a pack. The type operand is the indexed pack type. The
integer operand is an index into the components of this pack type; it
must be in range and resolve to a component that is not a pack
expansion.

Substitution must adjust the component index appropriately so that it
still refers to the same component. For example, if the pack type is
`Pack{repeat each T, Int}`, and substitution replaces `T` with
`Pack{Float, repeat each U}`, a component index of 1 must be adjusted to
2 so that it still refers to the `Int` element.

### pack_pack_index

```
sil-instruction ::= 'pack_pack_index' int-literal, sil-value 'of' sil-type
```

Produce the dynamic pack index of an element of a slice of a pack. The
type operand is the indexed pack type. The integer operand is an index
into the components of this pack type and must be in range. The value
operand is the index in the pack slice and must be another pack indexing
instruction. The pack slice starts at the given index and extends for a
number of components equal to the number of components in the indexed
pack type of the operand. The pack type induced from the indexed pack
type by this slice must have the same shape as the indexed pack type of
the operand.

Substitution must adjust the component index appropriately so that it
still refers to the same component. For example, if the pack type is
`Pack{repeat each T, Int}`, and substitution replaces `T` with
`Pack{Float, repeat each U}`, a component index of 1 must be adjusted to
2 so that the slice will continue to begin at the `Int` element.

Note how, in the example above, the slice does not contain any pack
expansions. (It is either empty or the singleton pack `Pack{Int}`.) This
is not typically how this instruction is used but can easily occur after
inlining or other type substitution.

### dynamic_pack_index

```
sil-instruction ::= 'dynamic_pack_index' sil-value 'of' sil-type
```

Produce the dynamic pack index of an unknown element of a pack. The type
operand is the indexed pack type. The value operand is a dynamic index
into the dynamic elements of the pack and must have type `Builtin.Word`.
The instruction has undefined behavior if the index is not in range for
the pack.

## Variadic Generics

### pack_length

```
sil-instruction ::= 'pack_length' sil-type
```

Produce the dynamic length of the given pack, which must be a formal
pack type. The value of the instruction has type `Builtin.Word`.

### open_pack_element

```
sil-instruction ::= 'open_pack_element' sil-value 'of' generic-parameter-list+ 'at' sil-apply-substitution-list ',' 'shape' sil-type ',' 'uuid' string-literal
```

Binds one or more opened pack element archetypes in the local type
environment.

The generic signature is the *generalization signature* of the pack
elements. This signature need not be related in any way to the generic
signature (if any) of the enclosing SIL function.

The `shape` type operand is resolved in the context of the
generalization signature. It must name a pack parameter. Archetypes will
be bound for all pack parameters with the same shape as this parameter.

The `uuid` operand must be an RFC 4122 UUID string, which is composed of
32 hex digits separated by hyphens in the pattern `8-4-4-4-12`. There
must not be any other `open_pack_element` instruction with this UUID in
the SIL function. Opened pack element archetypes are identified by this
UUID and are different from any other opened pack element archetypes in
the function, even if the operands otherwise match exactly.

The value operand is the pack index and must be the result of a pack
indexing instruction.

The substitution list matches the generalization signature and provides
contextual bindings for all of the type information there. As usual, the
substitutions for any pack parameters must be pack types. For pack
parameters with the same shape as the shape operand, these pack
substitutions must have the same shape as the indexed pack type of the
pack index operand (and therefore the same shape as each other).

The cost of this instruction is proportionate to the sum of the number
of pack parameters in the generalization signature with the same shape
as the shape type and the number of protocol conformance requirements
the generalization signature imposes on those parameters and their
associated types. If any of this information is not required for the
correct execution of the SIL function, simplifying the generalization
signature used by the`open_pack_element` can be a significant
optimization.

### pack_element_get

```
sil-instruction ::= 'pack_element_get' sil-value 'of' sil-operand 'as' sil-type

%addr = pack_element_get %index of %pack : $*Pack{Int, repeat each T} as $*Int
```

Extracts the value previously stored in a pack at a particular index. If
the pack element is uninitialized, this has undefined behavior.

Ownership is unclear for direct packs.

The first operand is the pack index and must be a pack indexing
instruction. The second operand is the pack and must be the address of a
pack value. The type operand is the projected element type of the pack
element and must be structurally well-typed for the given index and pack
type; see the structural type matching rules for pack indices.

### pack_element_set

```
sil-instruction ::= 'pack_element_set' sil-operand 'into' sil-value 'of' sil-operand

pack_element_set %addr : $*@pack_element("...") each U into %index of %pack : $*Pack{Int, repeat each T}
```

Places a value in a pack at a particular index.

Ownership is unclear for direct packs.

The first operand is the new element value. The second operand is the
pack index and must be a pack indexing instruction. The third operand is
the pack and must be the address of a pack value. The type of the
element value operand is the projected element type of the pack element
and must be structurally well-typed for the given index and pack type;
see the structural type matching rules for pack indices.

## Value Generics

### type_value

```
sil-instruction ::= 'type_value' sil-type 'for' sil-identifier
```

Produce the dynamic value of the given value generic, which must be a
formal value generic type. The value of the instruction has the type of
whatever the underlying value generic's type is. For right now that is
limited to `Int`.

## Unchecked Conversions

These instructions implement type conversions which are not checked.
These are either user-level conversions that are always safe and do not
need to be checked, or implementation detail conversions that are
unchecked for performance or flexibility.

### upcast

```
sil-instruction ::= 'upcast' sil-operand 'to' sil-type

%1 = upcast %0 : $D to $B
// $D and $B must be class types or metatypes, with B a superclass of D
// %1 will have type $B
```

Represents a conversion from a derived class instance or metatype to a
superclass, or from a base-class-constrained archetype to its base
class.

### address_to_pointer

```
sil-instruction ::= 'address_to_pointer' ('[' 'stack_protection' ']')? sil-operand 'to' sil-type

%1 = address_to_pointer %0 : $*T to $Builtin.RawPointer
// %0 must be of an address type $*T
// %1 will be of type Builtin.RawPointer
```

Creates a `Builtin.RawPointer` value corresponding to the address `%0`.
Converting the result pointer back to an address of the same type will
give an address equivalent to `%0`. It is undefined behavior to cast the
`RawPointer` to any address type other than its original address type or
any [layout compatible types](Types.md#layout-compatible-types).

The `stack_protection` flag indicates that stack protection is done for
the pointer origin.

### pointer_to_address

```
sil-instruction ::= 'pointer_to_address' sil-operand 'to' ('[' 'strict' ']')? ('[' 'invariant' ']')? ('[' 'alignment' '=' alignment ']')? sil-type
alignment ::= [0-9]+

%1 = pointer_to_address %0 : $Builtin.RawPointer to [strict] $*T
// %1 will be of type $*T
```

Creates an address value corresponding to the `Builtin.RawPointer` value
`%0`. Converting a `RawPointer` back to an address of the same type as
its originating [address_to_pointer](#address_to_pointer) instruction
gives back an equivalent address. It is undefined behavior to cast the
`RawPointer` back to any type other than its original address type or
[layout compatible types](Types.md#layout-compatible-types). It is also
undefined behavior to cast a `RawPointer` from a heap object to any
address type.

The `strict` flag indicates whether the returned address adheres to
strict aliasing. If true, then the type of each memory access dependent
on this address must be consistent with the memory's bound type. A
memory access from an address that is not strict cannot have its address
substituted with a strict address, even if other nearby memory accesses
at the same location are strict.

The `invariant` flag is set if loading from the returned address always
produces the same value.

The `alignment` integer value specifies the byte alignment of the
address. `alignment=0` is the default, indicating the natural alignment
of `T`.

### unchecked_ref_cast

```
sil-instruction ::= 'unchecked_ref_cast' sil-operand 'to' sil-type

%1 = unchecked_ref_cast %0 : $A to $B
// %0 must be an object of type $A
// $A must be a type with retainable pointer representation
// %1 will be of type $B
// $B must be a type with retainable pointer representation
```

Converts a heap object reference to another heap object reference type.
This conversion is unchecked, and it is undefined behavior if the
destination type is not a valid type for the heap object. The heap
object reference on either side of the cast may be a class existential,
and may be wrapped in one level of Optional.

### unchecked_ref_cast_addr

```
sil-instruction ::= 'unchecked_ref_cast_addr'
                    sil-type 'in' sil-operand 'to'
                    sil-type 'in' sil-operand

unchecked_ref_cast_addr $A in %0 : $*A to $B in %1 : $*B
// %0 must be the address of an object of type $A
// $A must be a type with retainable pointer representation
// %1 must be the address of storage for an object of type $B
// $B must be a retainable pointer representation
```

Loads a heap object reference from an address and stores it at the
address of another uninitialized heap object reference. The loaded
reference is always taken, and the stored reference is initialized. This
conversion is unchecked, and it is undefined behavior if the destination
type is not a valid type for the heap object. The heap object reference
on either side of the cast may be a class existential, and may be
wrapped in one level of Optional.

### unchecked_addr_cast

```
sil-instruction ::= 'unchecked_addr_cast' sil-operand 'to' sil-type

%1 = unchecked_addr_cast %0 : $*A to $*B
// %0 must be an address
// %1 will be of type $*B
```

Converts an address to a different address type. Using the resulting
address is undefined unless `B` is layout compatible with `A`. The
layout of `B` may be smaller than that of `A` as long as the lower order
bytes have identical layout.

### unchecked_trivial_bit_cast

```
sil-instruction ::= 'unchecked_trivial_bit_cast' sil-operand 'to' sil-type

%1 = unchecked_trivial_bit_cast %0 : $Builtin.NativeObject to $Builtin.Word
// %0 must be an object.
// %1 must be an object with trivial type.
```

Bitcasts an object of type `A` to be of same sized or smaller type `B`
with the constraint that `B` must be trivial. This can be used for
bitcasting among trivial types, but more importantly is a one way
bitcast from non-trivial types to trivial types.

### unchecked_bitwise_cast

```
sil-instruction ::= 'unchecked_bitwise_cast' sil-operand 'to' sil-type

%1 = unchecked_bitwise_cast %0 : $A to $B
```

Bitwise copies an object of type `A` into a new object of type `B` of
the same size or smaller.

### unchecked_value_cast

```
sil-instruction ::= 'unchecked_value_cast' sil-operand 'to' sil-type

%1 = unchecked_value_cast %0 : $A to $B
```

Bitwise copies an object of type `A` into a new layout-compatible object
of type `B` of the same size.

This instruction is assumed to forward a fixed ownership (set upon its
construction) and lowers to 'unchecked_bitwise_cast' in non-OSSA code.
This causes the cast to lose its guarantee of layout-compatibility.

### unchecked_ownership_conversion

```
sil-instruction ::= 'unchecked_ownership_conversion' sil-operand ',' sil-value-ownership-kind 'to' sil-value-ownership-kind

%1 = unchecked_ownership_conversion %0 : $A, @guaranteed to @owned
```

Converts its operand to an identical value of the same type but with
different ownership without performing any semantic operations normally
required by for ownership conversion.

This is used in Objective-C compatible destructors to convert a
guaranteed parameter to an owned parameter without performing a semantic
copy.

The resulting value must meet the usual ownership requirements; for
example, a trivial type must have '.none' ownership.

### ref_to_raw_pointer

```
sil-instruction ::= 'ref_to_raw_pointer' sil-operand 'to' sil-type

%1 = ref_to_raw_pointer %0 : $C to $Builtin.RawPointer
// $C must be a class type, or Builtin.NativeObject, or AnyObject
// %1 will be of type $Builtin.RawPointer
```

Converts a heap object reference to a `Builtin.RawPointer`. The
`RawPointer` result can be cast back to the originating class type but
does not have ownership semantics. It is undefined behavior to cast a
`RawPointer` from a heap object reference to an address using
[pointer_to_address](#pointer_to_address).

### raw_pointer_to_ref

```
sil-instruction ::= 'raw_pointer_to_ref' sil-operand 'to' sil-type

%1 = raw_pointer_to_ref %0 : $Builtin.RawPointer to $C
// $C must be a class type, or Builtin.NativeObject, or AnyObject
// %1 will be of type $C
```

Converts a `Builtin.RawPointer` back to a heap object reference. Casting
a heap object reference to `Builtin.RawPointer` back to the same type
gives an equivalent heap object reference (though the raw pointer has no
ownership semantics for the object on its own). It is undefined behavior
to cast a `RawPointer` to a type unrelated to the dynamic type of the
heap object. It is also undefined behavior to cast a `RawPointer` from
an address to any heap object type.

### ref_to_unowned

```
sil-instruction ::= 'ref_to_unowned' sil-operand

%1 = unowned_to_ref %0 : T
// $T must be a reference type
// %1 will have type $@unowned T
```

Adds the `@unowned` qualifier to the type of a reference to a heap
object. No runtime effect.

### unowned_to_ref

```
sil-instruction ::= 'unowned_to_ref' sil-operand

%1 = unowned_to_ref %0 : $@unowned T
// $T must be a reference type
// %1 will have type $T
```

Strips the `@unowned` qualifier off the type of a reference to a heap
object. No runtime effect.

### ref_to_unmanaged

TODO

### unmanaged_to_ref

TODO

### convert_function

```
sil-instruction ::= 'convert_function' sil-operand 'to'
                    ('[' 'without_actually_escaping' ']')?
                    sil-type

%1 = convert_function %0 : $T -> U to $T' -> U'
// %0 must be of a function type $T -> U ABI-compatible with $T' -> U'
//   (see below)
// %1 will be of type $T' -> U'
```

Performs a conversion of the function `%0` to type `T`, which must be
ABI-compatible with the type of `%0`. Function types are ABI-compatible
if their input and result types are tuple types that, after
destructuring, differ only in the following ways:

-   Corresponding tuple elements may add, remove, or change keyword
    names. `(a:Int, b:Float, UnicodeScalar) -> ()` and
    `(x:Int, Float, z:UnicodeScalar) -> ()` are ABI compatible.
-   A class tuple element of the destination type may be a superclass or
    subclass of the source type's corresponding tuple element.

The function types may also differ in attributes, except that the
`convention` attribute cannot be changed and the `@noescape` attribute
must not change for functions with context.

A `convert_function` cannot be used to change a thick type's
`@noescape` attribute (`@noescape` function types with context are not
ABI compatible with escaping function types with context) -- however,
thin function types with and without `@noescape` are ABI compatible
because they have no context. To convert from an escaping to a
`@noescape` thick function type use `convert_escape_to_noescape`.

With the `without_actually_escaping` attribute, the `convert_function`
may be used to convert a non-escaping closure into an escaping function
type. This attribute must be present whenever the closure operand has an
unboxed capture (via `@inout_aliasable`) *and* the resulting function
type is escaping. (This only happens as a result of
`withoutActuallyEscaping()`). If the attribute is present then the
resulting function type must be escaping, but the operand's function
type may or may not be @noescape. Note that a non-escaping closure may
have unboxed captured even though its SIL function type is "escaping".

### convert_escape_to_noescape

```
sil-instruction ::= 'convert_escape_to_noescape' sil-operand 'to' sil-type
%1 = convert_escape_to_noescape %0 : $T -> U to $@noescape T' -> U'
// %0 must be of a function type $T -> U ABI-compatible with $T' -> U'
//   (see convert_function)
// %1 will be of the trivial type $@noescape T -> U
```

Converts an escaping (non-trivial) function type to a `@noescape`
trivial function type. Something must guarantee the lifetime of the
input `%0` for the duration of the use `%1`.

A `convert_escape_to_noescape [not_guaranteed] %opd` indicates that the
lifetime of its operand was not guaranteed by SILGen and a mandatory
pass must be run to ensure the lifetime of `%opd` for the
conversion's uses.

A `convert_escape_to_noescape [escaped]` indicates that the result was
passed to a function (materializeForSet) which escapes the closure in a
way not expressed by the convert's users. The mandatory pass must
ensure the lifetime in a conservative way.

### thunk

```
sil-instruction ::= 'thunk' sil-thunk-attr* sil-value sil-apply-substitution-list? () sil-type
sil-thunk-attr ::= '[' thunk-kind ']'
sil-thunk-kind ::= identity

%1 = thunk [identity] %0() : $@convention(thin) (T) -> U
// %0 must be of a function type $T -> U
// %1 will be of type @callee_guaranteed (T) -> U since we are creating an
// "identity" thunk.

%1 = thunk [identity] %0<T>() : $@convention(thin) (τ_0_0) -> ()
// %0 must be of a function type $T -> ()
// %1 will be of type @callee_guaranteed <τ_0_0> (τ_0_0) -> () since we are creating a
// "identity" thunk.
```

Takes in a function and depending on the kind produces a new function
result that is `@callee_guaranteed`. The specific way that the function
type of the input is modified by this instruction depends on the
specific `sil-thunk-kind` of the instruction. So for instance, the
`hop_to_mainactor_if_needed` thunk just returns a callee_guaranteed
version of the input function... but one could imagine a
"reabstracted" thunk kind that would produce the appropriate
reabstracted thunk kind.

This instructions is lowered to a true think in Lowered SIL by the
ThunkLowering pass.

It is assumed that like [partial_apply](#partial_apply), if we need a
substitution map, it will be attached to `thunk`. This ensures
that we have the substitution map already created if we need to create a
[partial_apply](#partial_apply).

### classify_bridge_object

```
sil-instruction ::= 'classify_bridge_object' sil-operand

%1 = classify_bridge_object %0 : $Builtin.BridgeObject
// %1 will be of type (Builtin.Int1, Builtin.Int1)
```

Decodes the bit representation of the specified `Builtin.BridgeObject`
value, returning two bits: the first indicates whether the object is an
Objective-C object, the second indicates whether it is an Objective-C
tagged pointer value.

### value_to_bridge_object

```
sil-instruction ::= 'value_to_bridge_object' sil-operand

%1 = value_to_bridge_object %0 : $T
// %1 will be of type Builtin.BridgeObject
```

Sets the BridgeObject to a tagged pointer representation holding its
operands by tagging and shifting the operand if needed:

```
value_to_bridge_object %x ===
(x << _swift_abi_ObjCReservedLowBits) | _swift_BridgeObject_TaggedPointerBits
```

`%x` thus must not be using any high bits shifted away or the tag bits
post-shift. ARC operations on such tagged values are NOPs.

### ref_to_bridge_object

```
sil-instruction ::= 'ref_to_bridge_object' sil-operand, sil-operand

%2 = ref_to_bridge_object %0 : $C, %1 : $Builtin.Word
// %1 must be of reference type $C
// %2 will be of type Builtin.BridgeObject
```

Creates a `Builtin.BridgeObject` that references `%0`, with spare bits
in the pointer representation populated by bitwise-OR-ing in the value
of `%1`. It is undefined behavior if this bitwise OR operation affects
the reference identity of `%0`; in other words, after the following
instruction sequence:

```
%b = ref_to_bridge_object %r : $C, %w : $Builtin.Word
%r2 = bridge_object_to_ref %b : $Builtin.BridgeObject to $C
```

`%r` and `%r2` must be equivalent. In particular, it is assumed that
retaining or releasing the `BridgeObject` is equivalent to retaining or
releasing the original reference, and that the above
`ref_to_bridge_object` / `bridge_object_to_ref` round-trip can be folded
away to a no-op.

On platforms with ObjC interop, there is additionally a
platform-specific bit in the pointer representation of a `BridgeObject`
that is reserved to indicate whether the referenced object has native
Swift refcounting. It is undefined behavior to set this bit when the
first operand references an Objective-C object.

### bridge_object_to_ref

```
sil-instruction ::= 'bridge_object_to_ref' sil-operand 'to' sil-type

%1 = bridge_object_to_ref %0 : $Builtin.BridgeObject to $C
// $C must be a reference type
// %1 will be of type $C
```

Extracts the object reference from a `Builtin.BridgeObject`, masking out
any spare bits.

### bridge_object_to_word

```
sil-instruction ::= 'bridge_object_to_word' sil-operand 'to' sil-type

%1 = bridge_object_to_word %0 : $Builtin.BridgeObject to $Builtin.Word
// %1 will be of type $Builtin.Word
```

Provides the bit pattern of a `Builtin.BridgeObject` as an integer.

### thin_to_thick_function

```
sil-instruction ::= 'thin_to_thick_function' sil-operand 'to' sil-type

%1 = thin_to_thick_function %0 : $@convention(thin) T -> U to $T -> U
// %0 must be of a thin function type $@convention(thin) T -> U
// The destination type must be the corresponding thick function type
// %1 will be of type $T -> U
```

Converts a thin function value, that is, a bare function pointer with no
context information, into a thick function value with ignored context.
Applying the resulting thick function value is equivalent to applying
the original thin value. The `thin_to_thick_function` conversion may be
eliminated if the context is proven not to be needed.

### thick_to_objc_metatype

```
sil-instruction ::= 'thick_to_objc_metatype' sil-operand 'to' sil-type

%1 = thick_to_objc_metatype %0 : $@thick T.Type to $@objc_metatype T.Type
// %0 must be of a thick metatype type $@thick T.Type
// The destination type must be the corresponding Objective-C metatype type
// %1 will be of type $@objc_metatype T.Type
```

Converts a thick metatype to an Objective-C class metatype. `T` must be
of class, class protocol, or class protocol composition type.

### objc_to_thick_metatype

```
sil-instruction ::= 'objc_to_thick_metatype' sil-operand 'to' sil-type

%1 = objc_to_thick_metatype %0 : $@objc_metatype T.Type to $@thick T.Type
// %0 must be of an Objective-C metatype type $@objc_metatype T.Type
// The destination type must be the corresponding thick metatype type
// %1 will be of type $@thick T.Type
```

Converts an Objective-C class metatype to a thick metatype. `T` must be
of class, class protocol, or class protocol composition type.

### objc_metatype_to_object

TODO

### objc_existential_metatype_to_object

TODO

## Checked Conversions

Some user-level cast operations can fail and thus require runtime
checking.

The `unconditional_checked_cast_addr` and
[unconditional_checked_cast](#unconditional_checked_cast) instructions
performs an unconditional checked cast; it is a runtime failure if the
cast fails. The [checked_cast_addr_br](#checked_cast_addr_br) and
[checked_cast_br](#checked_cast_br) terminator instruction performs a
conditional checked cast; it branches to one of two destinations based
on whether the cast succeeds or not.

### unconditional_checked_cast

```
sil-instruction ::= 'unconditional_checked_cast' 
                    sil-prohibit-isolated-conformances?
                    sil-operand 'to' sil-type

%1 = unconditional_checked_cast %0 : $A to $B
%1 = unconditional_checked_cast %0 : $*A to $*B
// $A and $B must be both objects or both addresses
// %1 will be of type $B or $*B
```

Performs a checked scalar conversion, causing a runtime failure if the
conversion fails. Casts that require changing representation or
ownership are unsupported.

### unconditional_checked_cast_addr

```
sil-instruction ::= 'unconditional_checked_cast_addr'
                    sil-prohibit-isolated-conformances?
                    sil-type 'in' sil-operand 'to'
                    sil-type 'in' sil-operand

unconditional_checked_cast_addr $A in %0 : $*@thick A to $B in %1 : $*@thick B
// $A and $B must be both addresses
// %1 will be of type $*B
// $A is destroyed during the conversion. There is no implicit copy.
```

Performs a checked indirect conversion, causing a runtime failure if the
conversion fails.

## Runtime Failures

### cond_fail

```
sil-instruction ::= 'cond_fail' sil-operand, string-literal

cond_fail %0 : $Builtin.Int1, "failure reason"
// %0 must be of type $Builtin.Int1
```

This instruction produces a runtime failure if the
operand is 1. Execution proceeds normally if the operand is zero. The
second operand is a static failure message, which is displayed by the
debugger in case the failure is triggered.

## Terminators

These instructions terminate a basic block. Every basic block must end
with a terminator. Terminators may only appear as the final instruction
of a basic block.

### unreachable

```
sil-terminator ::= 'unreachable'

unreachable
```

Indicates that control flow must not reach the end of the current basic
block. It is a dataflow error if an unreachable terminator is reachable
from the entry point of a function and is not immediately preceded by an
`apply` of a no-return function.

### return

```
sil-terminator ::= 'return' sil-operand

return %0 : $T
// $T must be the return type of the current function
```

Exits the current function and returns control to the calling function.
If the current function was invoked with an `apply` instruction, the
result of that function will be the operand of this `return`
instruction. If the current function was invoked with a `try_apply`
instruction, control resumes at the normal destination, and the value of
the basic block argument will be the operand of this `return`
instruction.

If the current function is a single-yield coroutine (`yield_once` or
`yield_once_2`), there must not be a path from the entry block to a
`return` which does not pass through a `yield` instruction. This rule
does not apply in the `raw` SIL stage.

`return` does not retain or release its operand or any other values.

A function must not contain more than one `return` instruction.

### throw

```
sil-terminator ::= 'throw' sil-operand

throw %0 : $T
// $T must be the error result type of the current function
```

Exits the current function and returns control to the calling function.
The current function must have an error result, and so the function must
have been invoked with a `try_apply` instruction. Control will resume in
the error destination of that instruction, and the basic block argument
will be the operand of the `throw`.

`throw` does not retain or release its operand or any other values.

A function must not contain more than one `throw` instruction.

### throw_addr

```
sil-terminator ::= 'throw_addr'

throw_addr
// indirect error result must be initialized at this point
```

Exits the current function and returns control to the calling function.
The current function must have an indirect error result, and so the
function must have been invoked with a `try_apply` instruction. Control
will resume in the error destination of that instruction.

The function is responsible for initializing its error result before the
`throw_addr`.

`throw_addr` does not retain or release any values.

A function must not contain more than one `throw_addr` instruction.

### yield

```
sil-terminator ::= 'yield' sil-yield-values
                     ',' 'resume' sil-identifier
                     ',' 'unwind' sil-identifier
sil-yield-values ::= sil-operand
sil-yield-values ::= '(' (sil-operand (',' sil-operand)*)? ')'
```

Temporarily suspends the current function and provides the given values
to the calling function. The current function must be a coroutine, and
the yield values must match the yield types of the coroutine. If the
calling function resumes the coroutine normally, control passes to the
`resume` destination. If the calling function aborts the coroutine,
control passes to the `unwind` destination.

The `resume` and `unwind` destination blocks must be uniquely referenced
by the `yield` instruction. This prevents them from becoming critical
edges.

In a single-yield coroutine (`yield_once` or `yield_once_2`), there must
not be a control flow path leading from the `resume` edge to another
`yield` instruction in this function. This rule does not apply in the
`raw` SIL stage.

There must not be a control flow path leading from the `unwind` edge to
a `return` instruction, to a `throw` instruction, or to any block
reachable from the entry block via a path that does not pass through an
`unwind` edge. That is, the blocks reachable from `unwind` edges must
jointly form a disjoint subfunction of the coroutine.

### unwind

```
sil-terminator ::= 'unwind'
```

Exits the current function and returns control to the calling function,
completing an unwind from a `yield`. The current function must be a
coroutine.

`unwind` is only permitted in blocks reachable from the `unwind` edges
of `yield` instructions.

### br

```
sil-terminator ::= 'br' sil-identifier
                     '(' (sil-operand (',' sil-operand)*)? ')'

br label (%0 : $A, %1 : $B, ...)
// `label` must refer to a basic block label within the current function
// %0, %1, etc. must be of the types of `label`'s arguments
```

Unconditionally transfers control from the current basic block to the
block labeled `label`, binding the given values to the arguments of the
destination basic block.

### cond_br

```
sil-terminator ::= 'cond_br' sil-operand ','
                     sil-identifier '(' (sil-operand (',' sil-operand)*)? ')' ','
                     sil-identifier '(' (sil-operand (',' sil-operand)*)? ')'

cond_br %0 : $Builtin.Int1, true_label (%a : $A, %b : $B, ...), 
                               false_label (%x : $X, %y : $Y, ...)
// %0 must be of $Builtin.Int1 type
// `true_label` and `false_label` must refer to block labels within the
//   current function and must not be identical
// %a, %b, etc. must be of the types of `true_label`'s arguments
// %x, %y, etc. must be of the types of `false_label`'s arguments
```

Conditionally branches to `true_label` if `%0` is equal to `1` or to
`false_label` if `%0` is equal to `0`, binding the corresponding set of
values to the arguments of the chosen destination block.

In OSSA, `cond_br` must not have any arguments because in OSSA critical control
flow edges are not allowed.

### switch_value

```
sil-terminator ::= 'switch_value' sil-operand
                     (',' sil-switch-value-case)*
                     (',' sil-switch-default)?
sil-switch-value-case ::= 'case' sil-value ':' sil-identifier
sil-switch-default ::= 'default' sil-identifier

switch_value %0 : $Builtin.Int<n>, case %1: label1, 
                                   case %2: label2, 
                                   ...,            
                                   default labelN

// %0 must be a value of builtin integer type $Builtin.Int<n>
// `label1` through `labelN` must refer to block labels within the current
//   function
// FIXME: All destination labels currently must take no arguments
```

Conditionally branches to one of several destination basic blocks based
on a value of builtin integer. If the operand value matches one of the
`case` values of the instruction, control is transferred to the
corresponding basic block. If there is a `default` basic block, control
is transferred to it if the value does not match any of the `case`
values. It is undefined behavior if the value does not match any cases
and no `default` branch is provided.

### switch_enum

```
sil-terminator ::= 'switch_enum' sil-operand
                     (',' sil-switch-enum-case)*
                     (',' sil-switch-default)?
sil-switch-enum-case ::= 'case' sil-decl-ref ':' sil-identifier

switch_enum %0 : $U, case #U.Foo!enumelt: label1, 
                      case #U.Bar!enumelt: label2, 
                      ...,                 
                      default labelN

// %0 must be a value of enum type $U
// #U.Foo, #U.Bar, etc. must be 'case' declarations inside $U
// `label1` through `labelN` must refer to block labels within the current
//   function
// label1 must take either no basic block arguments, or a single argument
//   of the type of #U.Foo's data
// label2 must take either no basic block arguments, or a single argument
//   of the type of #U.Bar's data, etc.
// labelN must take no basic block arguments
```

Conditionally branches to one of several destination basic blocks based
on the discriminator in a loadable `enum` value. Unlike `switch_int`,
`switch_enum` requires coverage of the operand type: If the `enum` type
cannot be switched exhaustively in the current function, the `default`
branch is required; otherwise, the `default` branch is required unless a
destination is assigned to every `case` of the `enum`. The destination
basic block for a `case` may take an argument of the corresponding
`enum` `case`'s data type (or of the address type, if the operand is an
address). If the branch is taken, the destination's argument will be
bound to the associated data inside the original enum value. For
example:

```
enum Foo {
  case Nothing
  case OneInt(Int)
  case TwoInts(Int, Int)
}

sil @sum_of_foo : $Foo -> Int {
entry(%x : $Foo):
  switch_enum %x : $Foo,       
    case #Foo.Nothing!enumelt: nothing, 
    case #Foo.OneInt!enumelt:  one_int, 
    case #Foo.TwoInts!enumelt: two_ints

nothing:
  %zero = integer_literal $Int, 0
  return %zero : $Int

one_int(%y : $Int):
  return %y : $Int

two_ints(%ab : $(Int, Int)):
  %a = tuple_extract %ab : $(Int, Int), 0
  %b = tuple_extract %ab : $(Int, Int), 1
  %add = function_ref @add : $(Int, Int) -> Int
  %result = apply %add(%a, %b) : $(Int, Int) -> Int
  return %result : $Int
}
```

On a path dominated by a destination block of `switch_enum`, copying or
destroying the basic block argument has equivalent reference counting
semantics to copying or destroying the `switch_enum` operand:

```
// This retain_value...
retain_value %e1 : $Enum
switch_enum %e1, case #Enum.A: a, case #Enum.B: b
a(%a : $A):
// ...is balanced by this release_value
release_value %a
b(%b : $B):
// ...and this one
release_value %b
```

### switch_enum_addr

```
sil-terminator ::= 'switch_enum_addr' sil-operand
                     (',' sil-switch-enum-case)*
                     (',' sil-switch-default)?

switch_enum_addr %0 : $*U, case #U.Foo!enumelt: label1, 
                                        case #U.Bar!enumelt: label2, 
                                        ...,                 
                                        default labelN

// %0 must be the address of an enum type $*U
// #U.Foo, #U.Bar, etc. must be cases of $U
// `label1` through `labelN` must refer to block labels within the current
//   function
// The destinations must take no basic block arguments
```

Conditionally branches to one of several destination basic blocks based
on the discriminator in the enum value referenced by the address
operand.

Unlike `switch_int`, `switch_enum` requires coverage of the operand
type: If the `enum` type cannot be switched exhaustively in the current
function, the `default` branch is required; otherwise, the `default`
branch is required unless a destination is assigned to every `case` of
the `enum`. Unlike `switch_enum`, the payload value is not passed to the
destination basic blocks; it must be projected out separately with
[unchecked_take_enum_data_addr](#unchecked_take_enum_data_addr).

### dynamic_method_br

```
sil-terminator ::= 'dynamic_method_br' sil-operand ',' sil-decl-ref
                     ',' sil-identifier ',' sil-identifier

dynamic_method_br %0 : $P, #X.method, bb1, bb2
// %0 must be of protocol type
// #X.method must be a reference to an @objc method of any class
// or protocol type
```

Looks up the implementation of an Objective-C method with the same
selector as the named method for the dynamic type of the value inside an
existential container. The "self" operand of the result function value
is represented using an opaque type, the value for which must be
projected out as a value of type `Builtin.ObjCPointer`.

If the operand is determined to have the named method, this instruction
branches to `bb1`, passing it the uncurried function corresponding to
the method found. If the operand does not have the named method, this
instruction branches to `bb2`.

### checked_cast_br

```
sil-terminator ::= 'checked_cast_br' sil-checked-cast-exact?
                    sil-prohibit-isolated-conformances?
                    sil-type 'in'
                    sil-operand 'to' sil-type ','
                    sil-identifier ',' sil-identifier
sil-checked-cast-exact ::= '[' 'exact' ']'
sil-prohibit-isolated-conformances ::= '[' 'prohibit_isolated_conformances' ']'

checked_cast_br A in %0 : $A to $B, bb1, bb2
checked_cast_br *A in %0 : $*A to $*B, bb1, bb2
checked_cast_br [exact] A in %0 : $A to $A, bb1, bb2
// $A and $B must be both object types or both address types
// bb1 must take a single argument of type $B or $*B
// bb2 must take no arguments
```

Performs a checked scalar conversion from `$A` to `$B`. If the
conversion succeeds, control is transferred to `bb1`, and the result of
the cast is passed into `bb1` as an argument. If the conversion fails,
control is transferred to `bb2`.

An exact cast checks whether the dynamic type is exactly the target
type, not any possible subtype of it. The source and target types must
be class types.

A cast can specify that the runtime should prohibit all uses of isolated
conformances when attempting to satisfy protocol requirements of existentials.

### checked_cast_addr_br

```
sil-terminator ::= 'checked_cast_addr_br'
                    sil-prohibit-isolated-conformances?
                    sil-cast-consumption-kind
                    sil-type 'in' sil-operand 'to'
                    sil-stype 'in' sil-operand ','
                    sil-identifier ',' sil-identifier
sil-cast-consumption-kind ::= 'take_always'
sil-cast-consumption-kind ::= 'take_on_success'
sil-cast-consumption-kind ::= 'copy_on_success'

checked_cast_addr_br take_always $A in %0 : $*@thick A to $B in %2 : $*@thick B, bb1, bb2
// $A and $B must be both address types
// bb1 must take a single argument of type $*B
// bb2 must take no arguments
```

Performs a checked indirect conversion from `$A` to `$B`. If the
conversion succeeds, control is transferred to `bb1`, and the result of
the cast is left in the destination. If the conversion fails, control is
transferred to `bb2`.

### try_apply

```
sil-terminator ::= 'try_apply' sil-value
                      sil-apply-substitution-list?
                      '(' (sil-value (',' sil-value)*)? ')'
                      ':' sil-type
  'normal' sil-identifier, 'error' sil-identifier

try_apply %0(%1, %2, ...) : $(A, B, ...) -> (R, @error E),
  normal bb1, error bb2
bb1(%3 : R):
bb2(%4 : E):

// Note that the type of the callee '%0' is specified *after* the arguments
// %0 must be of a concrete function type $(A, B, ...) -> (R, @error E)
// %1, %2, etc. must be of the argument types $A, $B, etc.
```

Transfers control to the function specified by `%0`, passing it the
given arguments. When `%0` returns, control resumes in either the normal
destination (if it returns with `return`) or the error destination (if
it returns with `throw`).

`%0` must have a function type with an error result.

The rules on generic substitutions are identical to those of `apply`.

### await_async_continuation

```
sil-terminator ::= 'await_async_continuation' sil-value
                      ',' 'resume' sil-identifier
                      (',' 'error' sil-identifier)?

await_async_continuation %0 : $UnsafeContinuation<T>, resume bb1
await_async_continuation %0 : $UnsafeThrowingContinuation<T>, resume bb1, error bb2

bb1(%1 : @owned $T):
bb2(%2 : @owned $Error):
```

Suspends execution of an `@async` function until the continuation is
resumed. The continuation must be the result of a
`get_async_continuation` or `get_async_continuation_addr` instruction
within the same function; see the documentation for
`get_async_continuation` for discussion of further constraints on the IR
between `get_async_continuation[_addr]` and `await_async_continuation`.
This terminator can only appear inside an `@async` function. The
instruction must always have a `resume` successor, but must have an
`error` successor if and only if the operand is an
`UnsafeThrowingContinuation<T>`.

If the operand is the result of a `get_async_continuation` instruction,
then the `resume` successor block must take an argument whose type is
the maximally-abstracted lowered type of `T`, matching the type argument
of the `Unsafe[Throwing]Continuation<T>` operand. The value of the
`resume` argument is owned by the current function. If the operand is
the result of a `get_async_continuation_addr` instruction, then the
`resume` successor block must *not* take an argument; the resume value
will be written to the memory referenced by the operand to the
`get_async_continuation_addr` instruction, after which point the value
in that memory becomes owned by the current function. With either
variant, if the `await_async_continuation` instruction has an `error`
successor block, the `error` block must take a single `Error` argument,
and that argument is owned by the enclosing function. The memory
referenced by a `get_async_continuation_addr` instruction remains
uninitialized when `await_async_continuation` resumes on the `error`
successor.

It is possible for a continuation to be resumed before
`await_async_continuation`. In this case, the resume operation returns
immediately to its caller. When the `await_async_continuation`
instruction later executes, it then immediately transfers control to its
`resume` or `error` successor block, using the resume or error value
that the continuation was already resumed with.

## Differentiable Programming

### differentiable_function

```
sil-instruction ::= 'differentiable_function'
                    sil-differentiable-function-parameter-indices
                    sil-value ':' sil-type
                    sil-differentiable-function-derivative-functions-clause?

sil-differentiable-function-parameter-indices ::=
    '[' 'parameters' [0-9]+ (' ' [0-9]+)* ']'
sil-differentiable-derivative-functions-clause ::=
    'with_derivative'
    '{' sil-value ':' sil-type ',' sil-value ':' sil-type '}'

differentiable_function [parameters 0] %0 : $(T) -> T 
  with_derivative {%1 : $(T) -> (T, (T) -> T), %2 : $(T) -> (T, (T) -> T)}
```

Creates a `@differentiable` function from an original function operand
and derivative function operands (optional). There are two derivative
function kinds: a Jacobian-vector products (JVP) function and a
vector-Jacobian products (VJP) function.

`[parameters ...]` specifies parameter indices that the original
function is differentiable with respect to.

The `with_derivative` clause specifies the derivative function operands
associated with the original function.

The differentiation transformation canonicalizes all
`differentiable_function` instructions, generating
derivative functions if necessary to fill in derivative function
operands.

In raw SIL, the `with_derivative` clause is optional. In canonical SIL,
the `with_derivative` clause is mandatory.

### linear_function

```
sil-instruction ::= 'linear_function'
                    sil-linear-function-parameter-indices
                    sil-value ':' sil-type
                    sil-linear-function-transpose-function-clause?

sil-linear-function-parameter-indices ::=
    '[' 'parameters' [0-9]+ (' ' [0-9]+)* ']'
sil-linear-transpose-function-clause ::=
    with_transpose sil-value ':' sil-type

linear_function [parameters 0] %0 : $(T) -> T with_transpose %1 : $(T) -> T
```

Bundles a function with its transpose function into a
`@differentiable(_linear)` function.

`[parameters ...]` specifies parameter indices that the original
function is linear with respect to.

A `with_transpose` clause specifies the transpose function associated
with the original function. When a `with_transpose` clause is not
specified, the mandatory differentiation transform will add a
`with_transpose` clause to the instruction.

In raw SIL, the `with_transpose` clause is optional. In canonical SIL,
the `with_transpose` clause is mandatory.

### differentiable_function_extract

```
sil-instruction ::= 'differentiable_function_extract'
                    '[' sil-differentiable-function-extractee ']'
                    sil-value ':' sil-type
                    ('as' sil-type)?

sil-differentiable-function-extractee ::= 'original' | 'jvp' | 'vjp'

differentiable_function_extract [original] %0 : $@differentiable (T) -> T
differentiable_function_extract [jvp] %0 : $@differentiable (T) -> T
differentiable_function_extract [vjp] %0 : $@differentiable (T) -> T
differentiable_function_extract [jvp] %0 : $@differentiable (T) -> T 
  as $(@in_constant T) -> (T, (T.TangentVector) -> T.TangentVector)
```

Extracts the original function or a derivative function from the given
`@differentiable` function. The extractee is one of the following:
`[original]`, `[jvp]`, or `[vjp]`.

In lowered SIL, an explicit extractee type may be provided. This is
currently used by the LoadableByAddress transformation, which rewrites
function types.

### linear_function_extract

```
sil-instruction ::= 'linear_function_extract'
                    '[' sil-linear-function-extractee ']'
                    sil-value ':' sil-type

sil-linear-function-extractee ::= 'original' | 'transpose'

linear_function_extract [original] %0 : $@differentiable(_linear) (T) -> T
linear_function_extract [transpose] %0 : $@differentiable(_linear) (T) -> T
```

Extracts the original function or a transpose function from the given
`@differentiable(_linear)` function. The extractee is one of the
following: `[original]` or `[transpose]`.

### differentiability_witness_function

```
sil-instruction ::=
    'differentiability_witness_function'
    '[' sil-differentiability-witness-function-kind ']'
    '[' differentiability-kind ']'
    '[' 'parameters' sil-differentiability-witness-function-index-list ']'
    '[' 'results' sil-differentiability-witness-function-index-list ']'
    generic-parameter-clause?
    sil-function-name ':' sil-type

sil-differentiability-witness-function-kind ::= 'jvp' | 'vjp' | 'transpose'
sil-differentiability-witness-function-index-list ::= [0-9]+ (' ' [0-9]+)*

differentiability_witness_function [vjp] [reverse] [parameters 0] [results 0] 
  <T where T: Differentiable> @foo : $(T) -> T
```

Looks up a differentiability witness function (JVP, VJP, or transpose)
for a referenced function via SIL differentiability witnesses.

The differentiability witness function kind identifies the witness
function to look up: `[jvp]`, `[vjp]`, or `[transpose]`.

The remaining components identify the SIL differentiability witness:

-   Original function name.
-   Differentiability kind.
-   Parameter indices.
-   Result indices.
-   Witness generic parameter clause (optional). When parsing SIL, the
    parsed witness generic parameter clause is combined with the
    original function's generic signature to form the full witness
    generic signature.

## Optimizer Dataflow Marker Instructions

### mark_unresolved_non_copyable_value

```
sil-instruction ::= 'mark_unresolved_non_copyable_value'
                    '[' sil-optimizer-analysis-marker ']'

sil-optimizer-analysis-marker ::= 'consumable_and_assignable'
                              ::= 'no_consume_or_assign'
```

A canary value inserted by a SIL generating frontend to signal to the
move checker to check a specific value. Valid only in Raw SIL. The
relevant checkers should remove the
`mark_unresolved_non_copyable_value`
instruction after successfully running the relevant diagnostic. The idea
here is that instead of needing to introduce multiple "flagging"
instructions for the optimizer, we can just reuse this one instruction
by varying the kind.

If the sil optimizer analysis marker is `consumable_and_assignable` then
the move checker is told to check that the result of this instruction is
consumed at most once. If the marker is `no_consume_or_assign`, then the
move checker will validate that the result of this instruction is never
consumed or assigned over.

## No Implicit Copy and No Escape Value Instructions

### copyable_to_moveonlywrapper

```
sil-instruction ::= 'copyable_to_moveonlywrapper'
```

`copyable_to_moveonlywrapper` takes in a
`T` and maps it to a move only wrapped `@moveOnly T`. This is
semantically used by a code generator initializing a new moveOnly
binding from a copyable value. It semantically destroys its input
@owned value and returns a brand new independent @owned @moveOnly
value. It also is used to convert a trivial copyable value with type
'Trivial' into an owned non-trivial value of type '@moveOnly
Trivial'. If one thinks of '@moveOnly' as a monad, this is how one
injects a copyable value into the move only space.

### moveonlywrapper_to_copyable

```
sil-instruction ::= 'moveonlywrapper_to_copyable [owned]'
sil-instruction ::= 'moveonlywrapper_to_copyable [guaranteed]'
```

`moveonlywrapper_to_copyable` takes in a
`@moveOnly T` and produces a new `T` value. This is a 'forwarding'
instruction where at parse time, we only allow for one to choose it to
be [owned] or [guaranteed]. With time, we may eliminate the need for
the guaranteed form in the future.

-   `moveonlywrapper_to_copyable [owned]` is used to
    signal the end of lifetime of the '@moveOnly' wrapper. SILGen
    inserts these when ever a move only value has its ownership passed
    to a situation where a copyable value is needed. Since it is
    consuming, we know that the no implicit copy or no-escape checker
    will ensure that if we need a copy for it, the program will emit a
    diagnostic.
-   `moveonlywrapper_to_copyable [guaranteed]` is used to
    pass a @moveOnly T value as a copyable guaranteed parameter with
    type 'T' to a function. In the case of using no-implicit-copy
    checking this is always fine since no-implicit-copy is a local
    pattern. This would be an error when performing no escape checking.
    Importantly, this instruction also is where in the case of an
    @moveOnly trivial type, we convert from the non-trivial
    representation to the trivial representation.

### copyable_to_moveonlywrapper_addr

```
sil-instruction ::= 'copyable_to_moveonlywrapper_addr'
```

`copyable_to_moveonlywrapper_addr`
takes in a `*T` and maps it to a move only wrapped `*@moveOnly T`.
This is semantically used by a code generator initializing a new
moveOnly binding from a copyable value. It semantically acts as an
address cast. If one thinks of '@moveOnly' as a monad, this is how one
injects a copyable value into the move only space.

### moveonlywrapper_to_copyable_addr

```
sil-instruction ::= 'moveonlywrapper_to_copyable_addr'
```

`moveonlywrapper_to_copyable_addr`
takes in a `*@moveOnly T` and produces a new `*T` value. This
instruction acts like an address cast that projects out the underlying T
from an @moveOnly T.

NOTE: From the perspective of the address checker, a trivial
[load](#load) with a
`moveonlywrapper_to_copyable_addr`
operand is considered to be a use of a non-copyable type.

## Weak linking support

### has_symbol

``` none
sil-instruction ::= 'has_symbol' sil-decl-ref
```

Returns true if each of the underlying symbol addresses associated with
the given declaration are non-null. This can be used to determine
whether a weakly-imported declaration is available at runtime.

## Miscellaneous instructions

### ignored_use

```none
sil-instruction ::= 'ignored_use'
```

This instruction acts as a synthetic use instruction that suppresses unused
variable warnings. In Swift the equivalent operation is '_ = x'. This
importantly also provides a way to find the source location for '_ = x' when
emitting SIL diagnostics. It is only legal in Raw SIL and is removed as dead
code when we convert to Canonical SIL.

DISCUSSION: Before the introduction of this instruction, in certain cases,
SILGen would just not emit anything for '_ = x'... so one could not emit
diagnostics upon this case.
