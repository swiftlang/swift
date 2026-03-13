# Types in SIL

This document describes SIL types in detail. For an overview of SIL and OSSA
see the [SIL](SIL.md) document.

## Type Lowering

A *formal type* is the type of a value in Swift, such as an expression
result. Swift's formal type system intentionally abstracts over a large
number of representational issues like ownership transfer conventions
and directness of arguments. However, SIL aims to represent most such
implementation details, and so these differences deserve to be reflected
in the SIL type system. *Type lowering* is the process of turning a
formal type into its *lowered type*.

It is important to be aware that the lowered type of a declaration need
not be the lowered type of the formal type of that declaration. For
example, the lowered type of a declaration reference:

-   will usually be thin,
-   may have a non-Swift calling convention,
-   may use bridged types in its interface, and
-   may use ownership conventions that differ from Swift's default
    conventions.

### Abstraction Difference

Generic functions working with values of unconstrained type must
generally work with them indirectly, e.g. by allocating sufficient
memory for them and then passing around pointers to that memory.
Consider a generic function like this:

```
func generateArray<T>(n : Int, generator : () -> T) -> [T]
```

The function `generator` will be expected to store its result indirectly
into an address passed in an implicit parameter. There's really just no
reasonable alternative when working with a value of arbitrary type:

-   We don't want to generate a different copy of `generateArray` for
    every type `T`.
-   We don't want to give every type in the language a common
    representation.
-   We don't want to dynamically construct a call to `generator`
    depending on the type `T`.

But we also don't want the existence of the generic system to force
inefficiencies on non-generic code. For example, we'd like a function
of type `() -> Int` to be able to return its result directly; and yet,
`() -> Int` is a valid substitution of `() -> T`, and a caller of
`generateArray<Int>` should be able to pass an arbitrary `() -> Int` in
as the generator.

Therefore, the representation of a formal type in a generic context may
differ from the representation of a substitution of that formal type. We
call such differences *abstraction differences*.

SIL's type system is designed to make abstraction differences always
result in differences between SIL types. The goal is that a
properly-abstracted value should be correctly usable at any level of
substitution.

In order to achieve this, the formal type of a generic entity should
always be lowered using the abstraction pattern of its unsubstituted
formal type. For example, consider the following generic type:

```
struct Generator<T> {
  var fn : () -> T
}
var intGen : Generator<Int>
```

`intGen.fn` has the substituted formal type `() -> Int`, which would
normally lower to the type `@callee_owned () -> Int`, i.e. returning its
result directly. But if that type is properly lowered with the pattern
of its unsubstituted type `() -> T`, it becomes
`@callee_owned () -> @out Int`.

When a type is lowered using the abstraction pattern of an unrestricted
type, it is lowered as if the pattern were replaced with a type sharing
the same structure but replacing all materializable types with fresh
type variables.

For example, if `g` has type `Generator<(Int, Int) -> Float>`, `g.fn` is
lowered using the pattern `() -> T`, which eventually causes
`(Int, Int) -> Float` to be lowered using the pattern `T`, which is the
same as lowering it with the pattern `U -> V`; the result is that `g.fn`
has the following lowered type:

```
@callee_owned () -> @owned @callee_owned (@in (Int, Int)) -> @out Float.
```

As another example, suppose that `h` has type
`Generator<(Int, inout Int) -> Float>`. Neither `(Int, inout Int)` nor
`inout Int` are potential results of substitution because they aren't
materializable, so `h.fn` has the following lowered type:

```
@callee_owned () -> @owned @callee_owned (@in Int, @inout Int) -> @out Float
```

This system has the property that abstraction patterns are preserved
through repeated substitutions. That is, you can consider a lowered type
to encode an abstraction pattern; lowering `T` by `R` is equivalent to
lowering `T` by (`S` lowered by `R`).

SILGen has procedures for converting values between abstraction
patterns.

At present, only function and tuple types are changed by abstraction
differences.

### Legal SIL Types

A type `T` is a *legal SIL type* if:

-   it is a function type which satisfies the constraints (below) on
    function types in SIL,
-   it is a metatype type which describes its representation,
-   it is a tuple type whose element types are legal SIL types,
-   it is `Optional<U>`, where `U` is a legal SIL type,
-   it is a legal Swift type that is not a function, tuple, optional,
    metatype, or l-value type, or
-   it is a `@box` containing a legal SIL type.

Note that types in other recursive positions in the type grammar are
still formal types. For example, the instance type of a metatype or the
type arguments of a generic type are still formal Swift types, not
lowered SIL types.

### Box Types

Captured local variables and the payloads of `indirect` value types are
stored on the heap. The type `@box T` is a reference-counted type that
references a box containing a mutable value of type `T`. Boxes always
use Swift-native reference counting, so they can be queried for
uniqueness and cast to the `Builtin.NativeObject` type.

### Metatype Types

A concrete or existential metatype in SIL must describe its
representation. This can be:

-   `@thin`, meaning that it requires no storage and thus necessarily
    represents an exact type (only allowed for concrete metatypes);
-   `@thick`, meaning that it stores a reference to a type or (if a
    concrete class) a subclass of that type; or
-   `@objc`, meaning that it stores a reference to a class type (or a
    subclass thereof) using an Objective-C class object representation
    rather than the native Swift type-object representation.

### Function Types

Function types in SIL are different from function types in Swift in a
number of ways:

-   A SIL function type may be generic. For example, accessing a generic
    function with `function_ref` will give a value of generic function
    type.

-   A SIL function type may be declared `@noescape`. This is required
    for any function type passed to a parameter not declared with
    `@escaping` declaration modifier. `@noescape` function types may be
    either `@convention(thin)` or `@callee_guaranteed`. They have an
    unowned context - the context's lifetime must be independently
    guaranteed.

-   A SIL function type declares its conventional treatment of its
    context value:

    -   If it is `@convention(thin)`, the function requires no context
        value. Such types may also be declared `@noescape`, which
        trivially has no effect passing the context value.
    -   If it is `@callee_guaranteed`, the context value is treated as a
        direct parameter. This implies `@convention(thick)`. If the
        function type is also `@noescape`, then the context value is
        unowned, otherwise it is guaranteed.
    -   If it is `@callee_owned`, the context value is treated as an
        owned direct parameter. This implies `@convention(thick)` and is
        mutually exclusive with `@noescape`.
    -   If it is `@convention(block)`, the context value is treated as
        an unowned direct parameter.
    -   Other function type conventions are described in
        `Properties of Types` and `Calling Convention`.

-   SIL function types do not directly carry most of the actor-isolation
    information available in the Swift type system. Actor isolation is
    mostly simply erased from the SIL type system and treated as a
    dynamic property in SIL functions.

    However, `@isolated(any)` requires some additional ABI support and
    therefore must be carried on SIL function types. `@isolated(any)` is
    only allowed in combination with `@convention(thick)`; in
    particular, this precludes SIL function declarations from having
    `@isolated(any)` type. Instead, `@isolated(any)` function values are
    constructed with `partial_apply [isolated_any]`, which has
    additional requirements. The isolation of an `@isolated(any)`
    function can be read with the `function_extract_isolation`
    instruction.

-   A SIL function type declares the conventions for its parameters. The
    parameters are written as an unlabeled tuple; the elements of that
    tuple must be legal SIL types, optionally decorated with one of the
    following convention attributes.

    The value of an indirect parameter has type `*T`; the value of a
    direct parameter has type `T`.

    -   An `@in` parameter is indirect. The address must be of an
        initialized object; the function is responsible for destroying
        the value held there.
    -   An `@inout` parameter is indirect. The address must be of an
        initialized object. The memory must remain initialized for the
        duration of the call until the function returns. The function
        may mutate the pointee, and furthermore may weakly assume that
        there are no aliasing reads from or writes to the argument,
        though must preserve a valid value at the argument so that
        well-ordered aliasing violations do not compromise memory
        safety. This allows for optimizations such as local load and
        store propagation, introduction or elimination of temporary
        copies, and promotion of the `@inout` parameter to an `@owned`
        direct parameter and result pair, but does not admit "take"
        optimization out of the parameter or other optimization that
        would leave memory in an uninitialized state.
    -   An `@inout_aliasable` parameter is indirect. The address must be
        of an initialized object. The memory must remain initialized for
        the duration of the call until the function returns. The
        function may mutate the pointee, and must assume that other
        aliases may mutate it as well. These aliases however can be
        assumed to be well-typed and well-ordered; ill-typed accesses
        and data races to the parameter are still undefined.
    -   An `@owned` parameter is an owned direct parameter.
    -   A `@guaranteed` parameter is a guaranteed direct parameter.
    -   An `@in_guaranteed` parameter is indirect. The address must be
        of an initialized object; both the caller and callee promise not
        to mutate the pointee, allowing the callee to read it.
    -   An `@in_constant` parameter is indirect. The address must be of
        an initialized object; the function will treat the value held
        there as read-only.
    -   A `@pack_owned` parameter is indirect. The parameter must be of
        pack type and is always an address. Whether the pack elements
        are direct values or addresses of values is encoded in the pack
        type. In either case, both the pack elements and their
        referenced storage (if they are addresses) must be initialized
        prior to the call. The callee is responsible for destroying the
        values and is permitted to modify both the pack elements and
        their referenced storage. The caller is not permitted to access
        either the pack or the referenced storage during the call.
    -   A `@pack_guaranteed` parameter is indirect. The parameter must
        be of pack type and is always an address. Whether the pack
        elements are direct values or addresses of values is encoded in
        the pack type. In either case, both the pack elements and their
        referenced storage (if they are addresses) must be initialized
        prior to the call. Neither the callee nor the caller is
        permitted to modify or destroy the pack elements or their
        referenced storage during the call.
    -   A `@pack_inout` parameter is indirect. The parameter must be of
        pack type and is always an address. The pack elements must also
        always be addresses. The element addresses are set in the pack
        prior to the call, and the same addresses must be in the pack
        following the call, but the callee is permitted to modify the
        pack on a temporary basis if it wishes. The referenced storage
        of each element address must be initialized prior to the call,
        and it must still be initialized after the call, but the callee
        may modify the value stored there and potentially even leave it
        temporarily uninitialized. The caller is not permitted to access
        either the pack elements or their referenced storage during the
        call.
    -   Otherwise, the parameter is an unowned direct parameter.

-   A SIL function type declares the conventions for its results. The
    results are written as an unlabeled tuple; the elements of that
    tuple must be legal SIL types, optionally decorated with one of the
    following convention attributes. Indirect and direct results may be
    interleaved.

    Indirect results correspond to implicit arguments of type `*T` in
    function entry blocks and in the arguments to `apply` and
    `try_apply` instructions. These arguments appear in the order in
    which they appear in the result list, always before any parameters.

    Direct results correspond to direct return values of type `T`. A SIL
    function type has a `return type` derived from its direct results in
    the following way: when there is a single direct result, the return
    type is the type of that result; otherwise, it is the tuple type of
    the types of all the direct results, in the order they appear in the
    results list. The return type is the type of the operand of `return`
    instructions, the type of `apply` instructions, and the type of the
    normal result of `try_apply` instructions.

    -   An `@out` result is indirect.

        If the result type is not a pack type, then the address must be
        of an uninitialized object, and the callee is required to leave
        an initialized value there unless it terminates with a `throw`
        or has a non-Swift calling convention.

        If the result type is a pack type, then the pack must contain
        addresses. The addresses must be set in the pack prior to the
        call, and these same addresses must be in the pack after the
        call, but the callee may modify the pack elements on a temporary
        basis if it wishes. The addresses must be of uninitialized
        objects, and the callee is require to initialize them unless it
        terminates with a `throw` or has a non-Swift calling convention.

    -   An `@owned` result is an owned direct result.

    -   An `@autoreleased` result is an autoreleased direct result. If
        there is an autoreleased result, it must be the only direct
        result.

    -   Otherwise, the parameter is an unowned direct result.

A direct parameter, yield, or result of trivial type must always be
unowned.

A parameter or yield of pack type must always use one of the three pack
conventions. A result of pack type must always be `@out`.

An owned direct parameter or result is transferred to the recipient,
which becomes responsible for destroying the value. This means that the
value is passed at +1.

An unowned direct parameter or result is instantaneously valid at the
point of transfer. The recipient does not need to worry about race
conditions immediately destroying the value, but should copy it (e.g. by
`strong_retain`ing an object pointer) if the value will be needed sooner
rather than later.

A guaranteed direct parameter is like an unowned direct parameter value,
except that it is guaranteed by the caller to remain valid throughout
the execution of the call. This means that any `strong_retain`,
`strong_release` pairs in the callee on the argument can be eliminated.

An autoreleased direct result must have a type with a retainable pointer
representation. Autoreleased results are nominally transferred at +0,
but the runtime takes steps to ensure that a +1 can be safely
transferred, and those steps require precise code-layout control.
Accordingly, the SIL pattern for an autoreleased convention looks
exactly like the SIL pattern for an owned convention, and the extra
runtime instrumentation is inserted on both sides when the SIL is
lowered into LLVM IR. An autoreleased `apply` of a function that is
defined with an autoreleased result has the effect of a +1 transfer of
the result. An autoreleased `apply` of a function that is not defined
with an autoreleased result has the effect of performing a strong retain
in the caller. A non-autoreleased `apply` of a function that is defined
with an autoreleased result has the effect of performing an autorelease
in the callee.

-   SIL function types may provide an optional direct error result,
    written by placing `@error` on a result. A direct error result is
    always implicitly `@owned`. Only functions with a native calling
    convention may have an error result.

    A function with an error result cannot be called with `apply`. It
    must be called with `try_apply`. There is one exception to this
    rule: a function with an error result can be called with
    `apply [nothrow]` if the compiler can prove that the function does
    not actually throw.

    `return` produces a normal result of the function. To return an
    error result, use `throw`.

    Type lowering lowers the `throws` annotation on formal function
    types into more concrete error propagation:

    -   For native Swift functions, `throws` is turned into an error
        result.
    -   For non-native Swift functions, `throws` is turned in an
        explicit error-handling mechanism based on the imported API. The
        importer only imports non-native methods and types as `throws`
        when it is possible to do this automatically.

-   SIL function types may provide a pattern signature and substitutions
    to express that values of the type use a particular generic
    abstraction pattern. Both must be provided together. If a pattern
    signature is present, the component types (parameters, yields, and
    results) must be expressed in terms of the generic parameters of
    that signature. The pattern substitutions should be expressed in
    terms of the generic parameters of the overall generic signature, if
    any, or else the enclosing generic context, if any.

    A pattern signature follows the `@substituted` attribute, which must
    be the final attribute preceding the function type. Pattern
    substitutions follow the function type, preceded by the `for`
    keyword. For example:

    ```
    @substituted <T: Collection> (@in T) -> @out T.Element for Array<Int>
    ```

    The low-level representation of a value of this type may not match
    the representation of a value of the substituted-through version of
    it:

    ```
    (@in Array<Int>) -> @out Int
    ```

    Substitution differences at the outermost level of a function value
    may be adjusted using the `convert_function` instruction. Note that
    this only works at the outermost level and not in nested positions.
    For example, a function which takes a parameter of the first type
    above cannot be converted by `convert_function` to a function which
    takes a parameter of the second type; such a conversion must be done
    with a thunk.

    Type substitution on a function type with a pattern signature and
    substitutions only substitutes into the substitutions; the component
    types are preserved with their exact original structure.

-   In the implementation, a SIL function type may also carry
    substitutions for its generic signature. This is a convenience for
    working with applied generic types and is not generally a formal
    part of the SIL language; in particular, values should not have such
    types. Such a type behaves like a non-generic type, as if the
    substitutions were actually applied to the underlying function type.

-   SIL functions may optionally mark a function parameter as
    `@sil_isolated`. An `@sil_isolated` parameter must be one of:

    -   An actor or any actor type.
    -   A generic type that conforms to Actor or AnyActor.

    and must be the actor instance that a function is isolated to.
    Importantly this means that global actor isolated nominal types are
    never `@sil_isolated`. Only one parameter can ever be marked as
    `@sil_isolated` since a function cannot be isolated to multiple
    actors at the same time.

-   SIL functions may optionally mark a function parameter as
    `@sil_implicit_leading_param`. A SIL generator places this on a parameter
    that is used to represent a parameter that is implicitly generated by the
    generator at a full call site. Since they can only appear at the full call
    site, a `@sil_implicit_leading_param` can only appear in between the
    indirect result parameters and the direct parameters. For example the
    following swift code that uses `nonisolated(nonsending)`:

    ```
    nonisolated(nonsending)
    func f(_ x: DirectParam) -> IndirectResult {
      ...
    }
    ```

    would translate to the following SIL:

    ```
      sil [ossa] @f : $@convention(thin) @async (
         @sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>,
         @guaranteed DirectParam
      ) -> @out IndirectResult {
      bb0(%0 : $*IndirectResult, %1 : @guaranteed $Optional<Actor>, %2 : @guaranteed $DirectParam):
        ... use %0 ...
      }
    ```

    Notice how there is an `@sil_isolated` `@sil_implicit_leading_param`
    parameter that was inserted by SILGen to implicitly take in the caller's
    actor of f.

    NOTE: By design SILOptimizer passes should ignore
    `@sil_implicit_leading_param`. Instead, it should only be analyzed as a
    normal parameter. So as an example, in f above the implicit parameter should
    be treated by the optimizer just as any other isolated parameter. This is
    solely SILGen using SIL as a data structure. TODO: Can this be removed by
    SILGen so SILOptimizer passes cannot even see it?

### Async Functions

SIL function types may be `@async`. `@async` functions run inside async
tasks, and can have explicit *suspend points* where they suspend
execution. `@async` functions can only be called from other `@async`
functions, but otherwise can be invoked with the normal `apply` and
`try_apply` instructions (or `begin_apply` if they are coroutines).

In Swift, the `withUnsafeContinuation` primitive is used to implement
primitive suspend points. In SIL, `@async` functions represent this
abstraction using the `get_async_continuation[_addr]` and
`await_async_continuation` instructions. `get_async_continuation[_addr]`
accesses a *continuation* value that can be used to resume the coroutine
after it suspends. The resulting continuation value can then be passed
into a completion handler, registered with an event loop, or scheduled
by some other mechanism. Operations on the continuation can resume the
async function's execution by passing a value back to the async
function, or passing in an error that propagates as an error in the
async function's context. The `await_async_continuation` instruction
suspends execution of the coroutine until the continuation is invoked to
resume it. A use of `withUnsafeContinuation` in Swift:

```
func waitForCallback() async -> Int {
  return await withUnsafeContinuation { cc in
    registerCallback { cc.resume($0) }
  }
}
```

might lower to the following SIL:

```
sil @waitForCallback : $@convention(thin) @async () -> Int {
entry:
  %cc = get_async_continuation $Int
  %closure = function_ref @waitForCallback_closure
    : $@convention(thin) (UnsafeContinuation<Int>) -> ()
  apply %closure(%cc)
  await_async_continuation %cc, resume resume_cc

resume_cc(%result : $Int):
  return %result
}
```

The closure may then be inlined into the `waitForCallback` function:

```
sil @waitForCallback : $@convention(thin) @async () -> Int {
entry:
  %cc = get_async_continuation $Int
  %registerCallback = function_ref @registerCallback
    : $@convention(thin) (@convention(thick) () -> ()) -> ()
  %callback_fn = function_ref @waitForCallback_callback
  %callback = partial_apply %callback_fn(%cc)
  apply %registerCallback(%callback)
  await_async_continuation %cc, resume resume_cc

resume_cc(%result : $Int):
  return %result
}
```

Every continuation value must be used exactly once to resume its
associated async coroutine once. It is undefined behavior to attempt to
resume the same continuation more than once. On the flip side, failing
to resume a continuation will leave the async task stuck in the
suspended state, leaking any memory or other resources it owns.

### Coroutine Types

A coroutine is a function which can suspend itself and return control to
its caller without terminating the function. That is, it does not need
to obey a strict stack discipline. SIL coroutines have control flow that
is tightly integrated with their callers, and they pass information back
and forth between caller and callee in a structured way through yield
points. *Generalized accessors* and *generators* in Swift fit this
description: a `read` or `modify` accessor coroutine projects a single
value, yields ownership of that one value temporarily to the caller, and
then takes ownership back when resumed, allowing the coroutine to clean
up resources or otherwise react to mutations done by the caller.
*Generators* similarly yield a stream of values one at a time to their
caller, temporarily yielding ownership of each value in turn to the
caller. The tight coupling of the caller's control flow with these
coroutines allows the caller to *borrow* values produced by the
coroutine, where a normal function return would need to transfer
ownership of its return value, since a normal function's context ceases
to exist and be able to maintain ownership of the value after it
returns.

To support these concepts, SIL supports two flavors: single-yield and
multi-yield. These two flavors correspond to three kinds. A multi-yield
coroutine is of kind `@yield_many`. A single-yield coroutine is of kind
either `@yield_once` or `@yield_once_2`. Any of these attributes may be
written before a function type to indicate that it is a coroutine type.

Both single-yield and multi-yield coroutines are allowed to also be
`@async`. (Note that `@async` functions are not themselves modeled
explicitly as coroutines in SIL, although the implementation may use a
coroutine lowering strategy.)

A coroutine type may declare any number of *yielded values*, which is to
say, values which are provided to the caller at a yield point. Yielded
values are written in the result list of a function type, prefixed by
the `@yields` attribute. A yielded value may have a convention
attribute, taken from the set of parameter attributes and interpreted as
if the yield site were calling back to the calling function.

In addition to yielded values a coroutine could also have normal
results.

Coroutine functions may be used in many of the same ways as normal
function values. However, they cannot be called with the standard
`apply` or `try_apply` instructions. A non-throwing yield-once coroutine
can be called with the `begin_apply` instruction. There is no support
yet for calling a throwing yield-once coroutine or for calling a
yield-many coroutine of any kind.

Coroutines may contain the special `yield` and `unwind` instructions.

A multi-yield (`@yield_many`) coroutine may yield as many times as it
desires. A single-yield (`@yield_once` or `@yield_once_2`) coroutine may
yield exactly once before returning, although it may also `throw` before
reaching that point.

### Variadic Generics

Swift's variadic generics feature introduces the concepts of pack
parameters, pack arguments, and pack expansions. When these features are
used in formal types embedded in SIL, they follow the same rules as they
do in Swift. However, in its own type system and operations, SIL largely
uses a different (if closely related) language model.

#### Pack types

In (current) Swift, packs only exist as parameters, either type
parameters or value parameters. These parameters can then only be used
in pack expansions, which can only appear in certain naturally-variadic
positions, such as the elements list of a tuple type or the arguments
list of a call expression. Formally, substitution flattens these pack
expansions into the surrounding structure.

This language model poses similar problems for direct implementation at
runtime as many of Swift's other generics features. Normal compilation
paths (without unportable assembly-level heroics) require functions to
take a fixed list of parameters. Calling a generic function with packs
of different lengths cannot result in different parameters being mapped
to different registers (or positions in the stack arguments area). SIL
must therefore organize pack expansions in function parameters and
results into *concrete* variadic packs that can be passed with a fixed
ABI.

SIL must also directly model *temporary* packs, not just pack
parameters. After all, a pack parameter must be bound to something
concretely provided by the caller.

Packs are therefore something closer to a first-class type in the SIL
type system. A pack type is written with the syntax `Pack { ... }`. By
default, a pack type is *indirect*, meaning that its elements are
addresses. SIL does not currently support direct packs, but the
description in this document tries to leave room for them.

Pack types are always address-only and are always passed around by
address. Values of pack type cannot be moved or copied except by
explicitly iterating over the pack elements. They are allocated and
deallocated with special instructions.

Pack types are only allowed in two positions: - at the top level of a
type, e.g. `%0 : $Pack{Int, Float}` - as a parameter or result type of a
function type, e.g.
`%fn : $@convention(thin) (@pack_in Pack{Int, Float}) -> ()`

Note in particular that they are not allowed in tuple types. Pack
expansions in tuple types are still flattened into the surrounding tuple
structure like they are in Swift (unless the tuple is exploded, as
tuples normally are in function parameters or results). There are
specific instructions for manipulating tuples with variadic elements.

This explicit pack syntax can also be used to delimit type argument
packs in positions that expect formal types, such as the substitution
list of an `apply` instruction. This is necessary because SIL functions
can be parameterized over multiple packs, and unlike in Swift, the type
arguments to such functions are explicit on calls. If Swift ever gains
syntax to delimit packs in type argument lists, the SIL syntax will
switch to use it. Other features of SIL pack types, such as direct-ness,
are not allowed in these positions; a formal pack type is purely a list
of types and type expansions.

#### Pack expansions

Pack expansions (`repeat`) are allowed in a reduced set of situations in
lowered types: - the elements list of a tuple type - the elements list
of a pack type Note in particular that pack expansions cannot appear in
the parameters list or results list of a lowered function type. The
function type must traffic in packs explicitly.

If substitution into a lowered tuple type that is not a single unlabeled
element that is not a pack expansion would produce a tuple with a single
unlabeled element that is not a pack expansion, it actually produces
that element type. Certain instructions must be rewritten to accommodate
this when cloned under substitution.

#### Opened pack element archetypes

SIL must be able to work directly with the element types of a pack with
statically unknown elements. For example, it might need to move each
element of a pack into a tuple. To do this, it must be able to give a
temporary name to the element type. This type is called an *opened pack
element archetype*. It is spelled like this:

```
   @pack_element("<uuid>") T
```

There must be an `open_pack_element` in the current SIL function with
the given UUID. The name `T` must resolve to a pack parameter within the
generic signature of this instruction, and that pack parameter must have
the same shape class in the signature as the opened shape class pack
parameter. The pack parameter is then translated to the corresponding
element archetype.

Opened pack element archetypes can appear in both formal and lowered
types. As with opened existential archetypes, the `open_pack_element`
which introduces an opened pack element archetype must dominate all uses
of the archetype.

The current SIL parser does not support references to opened element
archetypes prior to the `open_pack_element` instruction. This can occur
when basic blocks are not in dominance order. Fixing this will likely
require changes to the syntax, at least for forward references.

### Properties of Types

SIL classifies types into additional subgroups based on ABI stability
and generic constraints:

-   *Loadable types* are types with a fully exposed concrete
    representation:

    -   Reference types
    -   Builtin value types
    -   Fragile struct types in which all element types are loadable
    -   Tuple types in which all element types are loadable
    -   Class protocol types
    -   Archetypes constrained by a class protocol

    Values of loadable types are loaded and stored by loading and
    storing individual components of their representation. As a
    consequence:

    > -   values of loadable types can be loaded into SIL SSA values and
    >     stored from SSA values into memory without running any
    >     user-written code, although compiler-generated reference
    >     counting operations can happen.
    > -   values of loadable types can be take-initialized (moved
    >     between memory locations) with a bitwise copy.

    A *loadable aggregate type* is a tuple or struct type that is
    loadable.

    A *trivial type* is a loadable type with trivial value semantics.
    Values of trivial type can be loaded and stored without any retain
    or release operations and do not need to be destroyed.

-   *Runtime-sized types* are restricted value types for which the
    compiler does not know the size of the type statically:

    -   Resilient value types
    -   Fragile struct or tuple types that contain resilient types as
        elements at any depth
    -   Archetypes not constrained by a class protocol

-   *Address-only types* are restricted value types which cannot be
    loaded or otherwise worked with as SSA values:

    -   Runtime-sized types
    -   Non-class protocol types
    -   `@weak` types
    -   Types that can't satisfy the requirements for being loadable
        because they care about the exact location of their value in
        memory and need to run some user-written code when they are
        copied or moved. Most commonly, types "care" about the
        addresses of values because addresses of values are registered
        in some global data structure, or because values may contain
        pointers into themselves. For example:
        -   Addresses of values of Swift `@weak` types are registered in
            a global table. That table needs to be adjusted when a
            `@weak` value is copied or moved to a new address.
        -   A non-COW collection type with a heap allocation (like
            `std::vector` in C++) needs to allocate memory and copy the
            collection elements when the collection is copied.
        -   A non-COW string type that implements a small string
            optimization (like many implementations of `std::string` in
            C++) can contain a pointer into the value itself. That
            pointer needs to be recomputed when the string is copied or
            moved.

    Values of address-only type ("address-only values") must reside in
    memory and can only be referenced in SIL by address. Addresses of
    address-only values cannot be loaded from or stored to. SIL provides
    special instructions for indirectly manipulating address-only
    values, such as `copy_addr` and `destroy_addr`.

Some additional meaningful categories of type:

-   A *heap object reference* type is a type whose representation
    consists of a single strong-reference-counted pointer. This includes
    all class types, the `Builtin.NativeObject` and `AnyObject` types,
    and archetypes that conform to one or more class protocols.
-   A *reference type* is more general in that its low-level
    representation may include additional global pointers alongside a
    strong-reference-counted pointer. This includes all heap object
    reference types and adds thick function types and protocol/protocol
    composition types that conform to one or more class protocols. All
    reference types can be `retain`-ed and `release`-d. Reference types
    also have *ownership semantics* for their referenced heap object;
    see [Reference Counting](#reference-counting) below.
-   A type with *retainable pointer representation* is guaranteed to be
    compatible (in the C sense) with the Objective-C `id` type. The
    value at runtime may be `nil`. This includes classes, class
    metatypes, block functions, and class-bounded existentials with only
    Objective-C-compatible protocol constraints, as well as one level of
    `Optional` or `ImplicitlyUnwrappedOptional` applied to any of the
    above. Types with retainable pointer representation can be returned
    via the `@autoreleased` return convention.

SILGen does not always map Swift function types one-to-one to SIL
function types. Function types are transformed in order to encode
additional attributes:

-   The **convention** of the function, indicated by the

    ```
    @convention(convention)
    ```

    attribute. This is similar to the language-level `@convention`
    attribute, though SIL extends the set of supported conventions with
    additional distinctions not exposed at the language level:

    -   `@convention(thin)` indicates a "thin" function reference,
        which uses the Swift calling convention with no special "self"
        or "context" parameters.
    -   `@convention(thick)` indicates a "thick" function reference,
        which uses the Swift calling convention and carries a
        reference-counted context object used to represent captures or
        other state required by the function. This attribute is implied
        by `@callee_owned` or `@callee_guaranteed`.
    -   `@convention(block)` indicates an Objective-C compatible block
        reference. The function value is represented as a reference to
        the block object, which is an `id`-compatible Objective-C object
        that embeds its invocation function within the object. The
        invocation function uses the C calling convention.
    -   `@convention(c)` indicates a C function reference. The function
        value carries no context and uses the C calling convention.
    -   `@convention(objc_method)` indicates an Objective-C method
        implementation. The function uses the C calling convention, with
        the SIL-level `self` parameter (by SIL convention mapped to the
        final formal parameter) mapped to the `self` and `_cmd`
        arguments of the implementation.
    -   `@convention(method)` indicates a Swift instance method
        implementation. The function uses the Swift calling convention,
        using the special `self` parameter.
    -   `@convention(witness_method)` indicates a Swift protocol method
        implementation. The function's polymorphic convention is
        emitted in such a way as to guarantee that it is polymorphic
        across all possible implementors of the protocol.

### Layout Compatible Types

(This section applies only to Swift 1.0 and will hopefully be obviated
in future releases.)

SIL tries to be ignorant of the details of type layout, and low-level
bit-banging operations such as pointer casts are generally undefined.
However, as a concession to implementation convenience, some types are
allowed to be considered **layout compatible**. Type `T` is *layout
compatible* with type `U` iff:

-   an address of type `$*U` can be cast by
    `address_to_pointer`/`pointer_to_address` to `$*T` and a valid value
    of type `T` can be loaded out (or indirectly used, if `T` is
    address-only),
-   if `T` is a nontrivial type, then `retain_value`/`release_value` of
    the loaded `T` value is equivalent to `retain_value`/`release_value`
    of the original `U` value.

This is not always a commutative relationship; `T` can be
layout-compatible with `U` whereas `U` is not layout-compatible with
`T`. If the layout compatible relationship does extend both ways, `T`
and `U` are **commutatively layout compatible**. It is however always
transitive; if `T` is layout-compatible with `U` and `U` is
layout-compatible with `V`, then `T` is layout-compatible with `V`. All
types are layout-compatible with themselves.

The following types are considered layout-compatible:

-   `Builtin.RawPointer` is commutatively layout compatible with all
    heap object reference types, and `Optional` of heap object reference
    types. (Note that `RawPointer` is a trivial type, so does not have
    ownership semantics.)
-   `Builtin.RawPointer` is commutatively layout compatible with
    `Builtin.Word`.
-   Structs containing a single stored property are commutatively layout
    compatible with the type of that property.
-   A heap object reference is commutatively layout compatible with any
    type that can correctly reference the heap object. For instance,
    given a class `B` and a derived class `D` inheriting from `B`, a
    value of type `B` referencing an instance of type `D` is layout
    compatible with both `B` and `D`, as well as `Builtin.NativeObject`
    and `AnyObject`. It is not layout compatible with an unrelated class
    type `E`.
-   For payloaded enums, the payload type of the first payloaded case is
    layout-compatible with the enum (*not* commutatively).

## Non-Copyable Types

There are two kinds of "non-copyable" types in SIL: pure non-copyable types
that are never copyable and move only wrapped types that are the non-copyable
versions of copyable types. The invariant that values of non-copyable types
obey is that they can only be copied (e.x.: operand to a
[copy_value](Instructions.md#copy_value), `copy_addr [init]`) in Raw SIL.
Once we are in non-Raw SIL though (i.e.  Canonical and later SIL stages), a
program is ill formed if one copies a non-copyable type.

The reason why we have this special rule for non-copyable types is that this
allows for SIL code generators to insert copies and then have a later
guaranteed checker optimization pass recover the underlying move-only semantics
by reconstructing needed copies and removing unneeded copies using Ownership
SSA. If any such copies are actually needed according to Ownership SSA, the
checker pass emits a diagnostic stating that move semantics have been violated.
If such a diagnostic is emitted then the checker pass transforms all copies on
move only types to their explicit copy forms to ensure that once we leave the
diagnostic passes and enter canonical SIL, our "copy" invariant is maintained.
