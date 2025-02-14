# Function Attributes

This document is a reference guide for SIL function attributes. For an overview
of SIL and OSSA see the [SIL](SIL.md) document.

### Canonical SIL stage override

```
sil-function-attribute ::= '[canonical]'
```

The function is in canonical SIL even if the module is still in raw SIL.

### Ownership SSA

```
sil-function-attribute ::= '[ossa]'
```

The function is in OSSA (ownership SSA) form.

### Transparent functions

```
sil-function-attribute ::= '[transparent]'
```

Transparent functions are always inlined and don't keep their source
information when inlined.

### Thunks

```
sil-function-attribute ::= '[' sil-function-thunk ']'
sil-function-thunk ::= 'thunk'
sil-function-thunk ::= 'signature_optimized_thunk'
sil-function-thunk ::= 'reabstraction_thunk'
sil-function-thunk ::= 'back_deployed_thunk'
```

The function is a compiler generated thunk.

```
sil-function-attribute ::= '[without_actually_escaping]'
```

The function is a thunk for closures which are not actually escaping.

### Dynamically replaceable functions

```
sil-function-attribute ::= '[dynamically_replacable]'
```

The function can be replaced at runtime with a different implementation.
Optimizations must not assume anything about such a function, even if
the SIL of the function body is available.

```
sil-function-attribute ::= '[dynamic_replacement_for' identifier ']'
sil-function-attribute ::= '[objc_replacement_for' identifier ']'
```

Specifies for which function this function is a replacement.

### exact_self_class

```
sil-function-attribute ::= '[exact_self_class]'
```

The function is a designated initializers, where it is known that the
static type being allocated is the type of the class that defines the
designated initializer.

### Global variable initialization

```
sil-function-attribute ::= '[' sil-function-purpose ']'
sil-function-purpose ::= 'global_init'
```

A `global_init` function is used to access a global variable which needs to be
initialized the first time the accessor is called. The implied semantics of a
`global_init` function are:

-   side-effects can occur any time before the first invocation.
-   all calls to the same `global_init` function have the same
    side-effects.
-   any operation that may observe the initializer's side-effects
    must be preceded by a call to the initializer.

This is currently true if the function is an addressor that was lazily
generated from a global variable access. Note that the initialization
function itself does not need this attribute. It is private and only
called within the addressor.

```
sil-function-purpose ::= 'global_init_once_fn'
```

The actual initialization function for a global, which is called once by its
corresponding `global_init` function.

```
sil-function-purpose ::= 'lazy_getter'
```

The function is a getter of a lazy property for which the backing storage is an
`Optional` of the property's type. The getter contains a top-level
[`switch_enum`](Instructions.md#switch_enum) (or
[`switch_enum_addr`](Instructions.md#switch_enum_addr)), which tests if the
lazy property is already computed. In the `None`-case, the property is computed
and stored to the backing storage of the property.

After the first call of a lazy property getter, it is guaranteed that
the property is computed and consecutive calls always execute the
`Some`-case of the top-level [`switch_enum`](Instructions.md#switch_enum).

### Weakly imported functions

```
sil-function-attribute ::= '[weak_imported]'
```

Cross-module references to this function should always use weak linking.

### Stack protection

```
sil-function-attribute ::= '[stack_protection]'
```

Stack protectors are inserted into this function to detect stack related
buffer overflows.

### Availability

```
sil-function-attribute ::= '[available' sil-version-tuple ']'
sil-version-tuple ::= [0-9]+ ('.' [0-9]+)*
```

The minimal OS-version where the function is available.

### Function inlining control

```
sil-function-attribute ::= '[' sil-function-inlining ']'
sil-function-inlining ::= 'noinline'
```

The function is never inlined.

```
sil-function-inlining ::= 'always_inline'
```

The function is always inlined.

### Optimization modes

```
sil-function-attribute ::= '[' sil-function-optimization ']'
sil-function-inlining ::= 'Onone'
sil-function-inlining ::= 'Ospeed'
sil-function-inlining ::= 'Osize'
```

The function is optimized according to this attribute, overriding the
setting from the command line.

```
sil-function-attribute ::= '[' sil-function-effects ']'
sil-function-effects ::= 'readonly'
sil-function-effects ::= 'readnone'
sil-function-effects ::= 'readwrite'
sil-function-effects ::= 'releasenone'
```

The specified memory effects of the function.

### Semantic attributes

```
sil-function-attribute ::= '[_semantics "' [A-Za-z._0-9]+ '"]'
```

The specified high-level semantics of the function. The optimizer can
use this information to perform high-level optimizations before such
functions are inlined. For example, `Array` operations are annotated
with semantic attributes to let the optimizer perform redundant bounds
check elimination and similar optimizations.

### Pre-specialization

```
sil-function-attribute ::= '[_specialize "' [A-Za-z._0-9]+ '"]'
```

Specifies for which types specialized code should be generated.

### Clang node owner

```
sil-function-attribute ::= '[clang "' identifier '"]'
```

The clang node owner.

### Performance constraints

```
sil-function-attribute ::= '[' performance-constraint ']'
performance-constraint :: 'no_locks'
performance-constraint :: 'no_allocation'
```

Specifies the performance constraints for the function, which defines
which type of runtime functions are allowed to be called from the
function.

```
sil-function-attribute ::= '[perf_constraint]'
```

Specifies that the optimizer and IRGen must not add runtime calls which
are not in the function originally. This attribute is set for functions
with performance constraints or functions which are called from
functions with performance constraints.
