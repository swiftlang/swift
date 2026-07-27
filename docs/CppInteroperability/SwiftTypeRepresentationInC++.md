# Swift Type Representation In C++

This document describes in details how Swift types are represented in C++.
It also covers related topics, like debug info representation for Swift types in
C++.

## Type Categories

### Value Types

1) Primitive Swift types like `Int`, `Float` , `OpaquePointer`, `UnsafePointer<int>?` are mapped to primitive C++ types. `int` , `float`, `void *`, `int * _Nullable`.

* Debug info: Does C++ debug info suffices?

2) Non-resilient fixed-layout Swift value type, e.g. `String` is mapped to a C++ class that stores the value in opaque buffer inline, e.g.:

```c++
class swift::String {
  ...
  alignas(8) char buffer[24]; // Swift value is stored here.
}
```

* Debug info: ...


3) Resilient (or opaque layout) inline-allocated Swift value type small enough to fit into inline buffer. e.g `URL` is mapped to a C++ class that stores the value in opaque buffer inline, e.g.:

```c++
class Foundation::URL {
  ...
  uintptr_t pointer;         // pointer has alignment to compute buffer offset?
  alignas(N) char buffer[M]; // Swift value is stored here.
};
```

concrete examples:

```c++
// representation for buffer aligned at 8:
{/*pointer=*/0x3, ....}; // buffer is alignas(2^3 = 8)

// representation for buffer aligned at 16:
{/*pointer=*/0x4, ....}; // buffer is alignas(2^4 = 16)

// where pointer < 10 for inline stores.
```

* Debug info: ...


4) Resilient (or opaque layout) boxed Swift value , e.g. `SHA256` is mapped to a C++ class that stores the value boxed up on the heap, e.g.:

```c++
class CryptoKit::SHA256 {
  ...
  uintptr_t pointer;         // Swift value is stored on the heap pointed by this pointer.
  alignas(8) char buffer[8];
};
```

* Debug info: ...


5) Generic non-resilient fixed-layout Swift value type, e.g. `Array<Int>`, `String?`, is mapped to a C++ class that stores the value in opaque buffer inline, e.g.:

```c++
class swift::Array<swift::Int> {
  ...
  alignas(8) char buffer[8]; // Swift value is stored here.
}
```

* Debug info: ...


6) Generic opaque-layout / resilient / opaque-layout template type params Swift value type, e.g. `SHA256?`, is mapped to a C++ class that stores the value boxed up on the heap, e.g.:

```c++
class swift::Optional<CryptoKit::SHA256> {
  ...
  uintptr_t pointer;        // Swift value is stored on the heap pointed by this pointer.
  alignas(N) char buffer[M];
}
```

* Debug info: ...

### Class Types

Class type is mapped to a C++ class that has a pointer to the underlying Swift instance in the base class:

```c++
class BaseClass {
private:
  void *_opaquePointer; // Swift class instance pointer is stored here.
}; 
class Vehicle: public BaseClass {
public:
}
```

* Debug info: ...

### Existential Types

Error type is mapped to a specific C++ `swift::Error` class that stores the pointer to the error:

```c++
class Error {
private:
  void *_opaquePointer; // Swift error instance pointer is stored here.:
};
```

* Debug info: ...


Existential type. e.g. `any Hashable` maps to a C++ class that stores the opaque existential value (`swift::any<swift::Hashable>`):

```c++
class swift::OpaqueExistential {
  // approximate layout.
  alignas(8) char buffer[8*5]; // opaque existential is stored here (inline or boxed by Swift)
};
class swift::any<swift::Hashable>: public swift::OpaqueExistential {
};
```

**Inline vs outline storage.** Values that are bitwiseTakable and fit in
24 bytes are stored directly in `_buffer`. Larger values are heap-allocated
via `swift_allocBox`; the `HeapObject*` is stored in `_buffer[0]` and the
value pointer is recovered via `swift_projectBox`. The `_initializeWithValue`,
`_destroyValue`, and `_projectValue` methods check the VWT size/flags to
dispatch between the two paths.

**Method definitions.** The special member functions and helpers are declared
in `_SwiftCxxInteroperability.h` but defined out-of-line in the generated
scaffolding (emitted by `PrintSwiftToClangCoreScaffold.cpp`), because they
need the complete `ValueWitnessTable` type and ptrauth discriminators.

#### `swift::Any`

`swift::Any` is a zero-witness-table existential container. It inherits from
`SwiftExistentialType` and serves two purposes:

1. Base class for marker protocol wrappers (which have no witness tables).
2. Default template parameter for unconstrained primary associated types
   (e.g., `Container<Element = swift::Any>`).

```c++
class Any : public _impl::SwiftExistentialType<> {
protected:
  Any() noexcept : SwiftExistentialType(uninit_t{}) {}
};
```

C++ code cannot inspect values inside `Any` but can pass them back to Swift
APIs for unboxing.

#### Per-protocol wrapper classes

Each non-marker protocol `P` emits a `final` class inheriting from
`SwiftExistentialType<_impl::PTag>`. The tag struct's `WitnessTable`
member type indicates the protocol contributes one witness table slot.
The `_witnessTables` array is in the base class template, sized by the
number of WT-bearing tags in the pack.

```c++
// Tag struct in _impl namespace:
struct DrawableTag {
  using WitnessTable = const void *_Nonnull;
};

// Generated for: public protocol Drawable { func draw() -> Int }
class Drawable final : public swift::_impl::SwiftExistentialType<_impl::DrawableTag> {
public:
  swift::Int draw() const {
    struct _w { /* witness function signature */ };
    return _loadWitness<1, DISC, decltype(&_w::call)>(_witnessTables[0])(
        _type, _witnessTables[0], _projectValue());
  }
  Drawable(const Circle &value) noexcept;  // boxing constructor

private:
  Drawable() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
  friend class _impl::_impl_Drawable;
};
```

**Layout:** `[buffer: 3 * sizeof(void*)] [type metadata] [witness tables...]`
-- 40 bytes for a single-protocol existential on 64-bit platforms (one WT slot).
Protocol inheritance does NOT add witness tables; multiple WT slots only
arise from ad-hoc compositions like `any A & B`.

**No C++ inheritance between protocol wrappers.** Existential-to-existential
conversion (e.g., `Stylable` to `Drawable`) is a value copy into a new
container via `_initializeWithCopy`, not a pointer cast. Different protocol
compositions have different container sizes.

#### Marker protocols

Marker protocols (declared with `@_marker`) have no witness tables and no
protocol requirements. Their tag struct has `static constexpr bool IsMarker = true`
instead of a `WitnessTable` member. They emit as `final` subclasses of
`SwiftExistentialType` with a marker tag:

```c++
struct PriorityTag {
  static constexpr bool IsMarker = true;
};

class Priority final : public swift::_impl::SwiftExistentialType<_impl::PriorityTag> {
private:
  Priority() noexcept : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
  friend class _impl::_impl_Priority;
};
```

Note: inheriting from `Sendable` does NOT make a protocol a marker. Only
protocols with the explicit `@_marker` attribute are treated as markers.

#### Protocol requirement method dispatch

Protocol methods are emitted directly on the wrapper class. Each method
loads a witness function pointer from the witness table at a compile-time
offset, with ptrauth signing on arm64e, and calls it with `(type, wt, self)`:

```c++
swift::Int draw() const {
  struct _w { _w() = delete;
    static SWIFT_CALL swift::Int call(
        void *, const void *, SWIFT_CONTEXT void *); };
  return _loadWitness<1, DISC, decltype(&_w::call)>(_witnessTables[0])(
      _type, _witnessTables[0], _projectValue());
}
```

**Inherited methods** are flattened into the wrapper. If `Stylable: Drawable`,
the `Stylable` wrapper gets both `style()` and `draw()`. Inherited methods
use two-level witness table dispatch: load the base protocol's WT from the
base conformance slot in the derived WT, then dispatch through the base WT.

**Conversion methods** like `asDrawable()` are emitted for each direct base
protocol conformance. They copy the existential buffer into a new wrapper
with the base witness table extracted from the derived WT.

#### Primary associated types (PATs)

Protocols with primary associated types emit as class templates. The template
parameter is a compile-time type tag only -- it does not affect existential
container layout:

```c++
template <typename Element = swift::Any>
class Container final : public swift::_impl::SwiftExistentialType<_impl::ContainerTag> {
public:
  swift::Int count() const { /* witness dispatch */ }
private:
  friend class _impl::_impl_Container;
};
```

#### Existential boxing (C++ to Swift)

Per-conformance implicit conversion constructors allow C++ code to construct
existential wrappers from concrete types:

```c++
// Generated when Circle: Drawable in the same module
Drawable::Drawable(const Circle &value) noexcept
    : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {
  _type = swift::TypeMetadataTrait<Circle>::getTypeMetadata();
  _initializeWithValue(_impl::_impl_Circle::getOpaquePointer(value));
  _witnessTables[0] = reinterpret_cast<const void *>(_impl::$sCircleDrawableWP);
}
```

Boxing constructors are declared in the class body but defined out-of-line
(via `outOfLineDefinitionsOS`) to avoid forward declaration ordering issues --
the conforming type's `_impl` class and `TypeMetadataTrait` specialization
may not be declared until later in the header.

`std::convertible_to<T, Drawable>` provides a generic constraint for free --
C++ template code can constrain on convertibility without any custom concept.
No runtime `swift_conformsToProtocol` lookup is used; only same-module
conformances with statically known witness table symbols are supported.

#### Ad-hoc protocol compositions

Ad-hoc compositions like `any Drawable & Resizable` emit named wrapper
classes inheriting from `SwiftExistentialType<DrawableTag, ResizableTag>`.
The wrapper gets methods from all constituent protocols plus `asFoo()`
extraction methods:

```c++
class AnyDrawableAndResizable final
    : public swift::_impl::SwiftExistentialType<_impl::DrawableTag, _impl::ResizableTag> {
public:
  swift::Int draw() const { /* dispatch via _witnessTables[0] */ }
  bool resize(swift::Int factor) const { /* dispatch via _witnessTables[1] */ }

  Drawable asDrawable() const;
  Resizable asResizable() const;

private:
  AnyDrawableAndResizable() noexcept
      : SwiftExistentialType(typename SwiftExistentialType::uninit_t{}) {}
  friend class _impl::_impl_AnyDrawableAndResizable;
};
```

**Naming.** The class name is `Any` + sorted protocol names joined with
`And` (e.g., `AnyDrawableAndResizable`). The canonical protocol ordering
comes from `ExistentialLayout::getProtocols()` -- `any Drawable & Resizable`
and `any Resizable & Drawable` produce the same type and wrapper.

**WT ordering.** Witness tables in `_witnessTables[]` follow the same
canonical ordering. For `AnyDrawableAndResizable`, slot 0 is Drawable's
WT and slot 1 is Resizable's.

**Lazy emission.** Composition wrappers are emitted lazily by
`DeclAndTypePrinter::ensureCompositionEmitted()` when a function
signature first references the composition type. A `StringSet<>` ensures
each composition is emitted at most once per module header.

**`asFoo()` extraction.** Each constituent protocol gets an `asFoo()`
method that copies the existential buffer into a single-protocol
wrapper with the corresponding WT. These are needed because C++ has no
implicit base-to-derived conversion -- the generic subset conversion
operator (below) produces the base `SwiftExistentialType<Tag>`, not
the named wrapper class.

**No boxing constructors.** Composition wrappers do not have boxing
constructors. Compositions are obtained from Swift function returns.

##### Composition parameter convention

Swift functions taking a composition parameter use the base template
type in C++ rather than the named wrapper class:

```c++
// Swift: func drawAndResize(_ x: any Drawable & Resizable) -> Int
swift::Int drawAndResize(
    const swift::_impl::SwiftExistentialType<
        Module::_impl::DrawableTag, Module::_impl::ResizableTag>& x);
```

This allows implicit derived-to-base conversion from any wrapper that
inherits from the same base, including wider compositions. Return types
use the named wrapper class so callers get method access:

```c++
Module::AnyDrawableAndResizable makeDrawableAndResizable();
```

#### Inverse tags

Protocols that opt out of `Copyable` or `Escapable` via `~Copyable` or
`~Escapable` get inverse tags appended to their template parameter list:

```c++
// public protocol Resource: ~Copyable { func use() -> Int }
class Resource final
    : public swift::_impl::SwiftExistentialType<_impl::ResourceTag, swift::_impl::NonCopyable> {
  // ...
};
```

`NonCopyable` and `NonEscapable` are tag structs satisfying the
`InverseTag` concept. They contribute no WT slots. The `Traits` struct
template uses them to set `IsCopyable = false` / `IsEscapable = false`,
which disables copy/move special members and the subset conversion
operator.

Detection uses `ProtocolDecl::canConformTo(InvertibleProtocolKind)` --
a return value of `Never` means the protocol has opted out. For
compositions, `NonCopyable` is appended if any constituent protocol
opts out of `Copyable` (likewise for `NonEscapable`).

#### Generic subset conversion

`SwiftExistentialType<Tags...>` provides a templated conversion operator
for implicit narrowing from wider compositions to narrower ones:

```c++
template <typename... TargetTags>
  requires (sizeof...(TargetTags) < sizeof...(Tags) &&
            Traits::IsCopyable &&
            ((ProtocolTag<TargetTags> || MarkerTag<TargetTags> ||
              InverseTag<TargetTags>) && ...) &&
            ((std::is_same_v<TargetTags, Tags> || ...) && ...))
operator SwiftExistentialType<TargetTags...>() const noexcept;
```

The operator copies the existential buffer and selects WT slots using
`_wtIndexOf<T, Tags...>()`, a constexpr helper that finds a tag's
position in the source pack. The `IsCopyable` constraint prevents
narrowing non-copyable existentials.

**Conversion to Any.** When `TargetTags` is empty, the result is
`SwiftExistentialType<>` (i.e., `Any`). The constraint
`sizeof...(TargetTags) < sizeof...(Tags)` is satisfied as long as the
source has at least one tag.

**Class-bound existentials.** `SwiftClassExistentialType<Tags...>` has
an analogous operator that converts to `SwiftExistentialType<TargetTags...>`
(not `SwiftClassExistentialType`), because the result may not be
class-bound.

#### Class-bound protocol existentials

Protocols that inherit from `AnyObject` (or are declared as class-bound)
have a different, smaller existential container layout. Instead of the
three-word value buffer + type metadata used by opaque existentials, a
class-bound existential stores only a class pointer (since the value is
always a reference type):

```
Opaque existential:      [buffer: 24] [type: 8] [WT: 8] = 40 bytes
Class-bound existential: [class ptr: 8] [WT: 8]         = 16 bytes
```

These wrappers inherit from `SwiftClassExistentialType<RenderableTag>` instead of
`SwiftExistentialType`:

```c++
// Generated for: public protocol Renderable: AnyObject { func render() -> Int }
class Renderable final : public swift::_impl::SwiftClassExistentialType<_impl::RenderableTag> {
public:
  swift::Int render() const {
    struct _w { /* witness function signature */ };
    return _loadWitness<1, DISC, decltype(&_w::call)>(_witnessTables[0])(
        _getType(), _witnessTables[0], _projectValue());
  }
  Renderable(const Canvas &value) noexcept;  // boxing constructor

private:
  Renderable() noexcept : SwiftClassExistentialType(typename SwiftClassExistentialType::uninit_t{}) {}
  friend class _impl::_impl_Renderable;
};
```

Key differences from opaque existentials:

- **Lifecycle:** `swift_retain`/`swift_release` instead of VWT-delegated
  `initializeBufferWithCopyOfBuffer`/`destroy`. Always bitwise-takable.
- **Type metadata:** Recovered via `swift_getObjectType(_value)` which
  reads the isa pointer. No stored `_type` field.
- **Method dispatch:** Uses `_getType()` instead of `_type` in witness
  function calls.
- **Loadable direct-pass:** Class existentials are always loadable (16 bytes)
  and get `swift_interop_passDirect_` treatment in C function signatures,
  unlike opaque existentials which are always passed as `const void *`.
- **Boxing constructor:** Extracts the class pointer via
  `_impl_RefCountedClass::getOpaquePointer` and calls `swift_retain`,
  rather than packing a value buffer with VWT.

```c++
template <typename... Tags>
class SwiftClassExistentialType {
public:
  SwiftClassExistentialType(const SwiftClassExistentialType &other) noexcept;
  SwiftClassExistentialType(SwiftClassExistentialType &&other) noexcept;
  SwiftClassExistentialType &operator=(const SwiftClassExistentialType &other) noexcept;
  SwiftClassExistentialType &operator=(SwiftClassExistentialType &&other) noexcept;
  ~SwiftClassExistentialType() noexcept;

protected:
  struct uninit_t {};
  SwiftClassExistentialType(uninit_t) noexcept : _value(nullptr) {}
  void _initializeWithCopy(const SwiftClassExistentialType &src) noexcept;
  void *_projectValue() const noexcept;
  void *_getType() const noexcept;  // swift_getObjectType

  template <size_t EntryOffset, uint16_t PtrAuthDisc, typename FnTy>
  FnTy _loadWitness(const void *wt) const;

  void *_value;  // retained class pointer
};
```

#### Stdlib protocol conformance records

Types conforming to `Equatable`, `Hashable`, or `Comparable` get conformance
record specializations and free operator templates in the generated header.

**Conformance records** are template specializations that lazily resolve the
protocol witness table. The primary templates are declared in
`_SwiftCxxInteroperability.h`:

```c++
template<typename T> struct EquatableConformance {};
template<typename T> struct HashableConformance {};
template<typename T> struct ComparableConformance {};
```

For each same-module conformance, `ModuleContentsWriter::emitStdlibConformanceRecords`
emits a specialization in `globalScopeDefinitionsOS` (outside the module namespace,
since it specializes a `swift::` template):

```c++
template<>
struct swift::EquatableConformance<MyModule::Point> {
  static inline const void* getWitnessTable() {
    static const void *wt = swift::_impl::swift_getWitnessTable(
        MyModule::_impl::$s8MyModule5PointVSQAAMc,
        swift::TypeMetadataTrait<MyModule::Point>::getTypeMetadata(), nullptr);
    return wt;
  }
};
```

The `getWitnessTable()` method (not a static pointer) is required because
cross-module protocol witness tables are not emitted as standalone global
symbols (`$s...WP`). They are lazily instantiated at runtime via
`swift_getWitnessTable`. The conformance descriptor (`$s...Mc` suffix) IS a
global symbol and serves as the input. The `static const` local ensures the
runtime call happens at most once per type.

The conformance descriptor extern is emitted in `outOfLineDefinitionsOS`
(inside the module namespace) wrapped in `_impl`:

```c++
namespace MyModule {
namespace _impl {
SWIFT_EXTERN const char $s8MyModule5PointVSQAAMc[];
} // namespace _impl
} // namespace MyModule
```

**Free operator templates** are defined at global scope in
`_SwiftCxxInteroperability.h`, gated on `__cpp_concepts`:

```c++
template<typename T>
    requires requires { swift::EquatableConformance<T>::getWitnessTable(); }
bool operator==(const T& lhs, const T& rhs) noexcept {
  auto *wt = swift::EquatableConformance<T>::getWitnessTable();
  auto fn = swift::_impl::_loadWitnessFromTable<1, 38891, ...>(wt);
  auto *metadata = swift::TypeMetadataTrait<T>::getTypeMetadata();
  return fn(&lhs, &rhs, metadata, metadata, wt);
}
```

Operators are at global scope (not `namespace swift`) because C++ argument types
live in module namespaces (e.g., `MyModule::Point`), and operators inside
`namespace swift` would not be found by unqualified lookup.

**ADL using declarations** are emitted inside the module namespace by
`emitStdlibConformanceRecords` so that STL algorithms (`std::sort`,
`std::find`, etc.) can find the operators via argument-dependent lookup:

```c++
namespace MyModule {
#ifdef __cpp_concepts
using ::operator==;
using ::operator!=;
using ::operator<;
// ...
#endif
}
```

Without these, STL algorithms that use `==` or `<` on dependent types fail --
ADL searches the argument type's associated namespace (`MyModule`), not the
global scope.

**ABI-frozen witness table offsets and ptrauth discriminators:**

| Protocol | Operator | WT offset | Ptrauth disc |
|----------|----------|-----------|--------------|
| Equatable | `==` | 1 | 38891 |
| Comparable | `<` | 2 | 59511 |

`!=` is `!(lhs == rhs)`. `<=`, `>`, `>=` are derived from `<` and `==`.

#### Import context limitations

Existential wrappers (and class wrappers) are recognized and imported back
as Swift existential types only in **function parameter and return type**
positions. These paths have explicit `CxxRecordAsSwiftType` checks in
`importMethodType`, `importFunctionReturnType`, and
`importFunctionParamsAndReturnType`.

The general `VisitRecordType` path -- used for struct fields, local
variable types, typedef bodies, and other non-function contexts -- does
**not** perform this check. `importDecl` returns `nullptr` for any
`CxxRecordDecl` with `CxxRecordSemanticsKind::SwiftExistentialType` (or
`SwiftClassType`), so the type fails to import in those positions.

This is a **pre-existing limitation** shared with the class interop path
(see the FIXME at `ImportDecl.cpp:3367`). Lifting it requires adding a
`CxxRecordAsSwiftType` lookup inside `VisitRecordType` itself, which
would fix both class and existential wrappers in all type positions.
This will likely need to be addressed for class template specialization
round-tripping, where C++ wrapper types appear as template arguments and
struct fields.

* Debug info: ...
