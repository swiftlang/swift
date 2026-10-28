# COM Interoperability

Swift's COM interoperability maps COM interfaces to protocols, COM interface
pointers to one-word protocol existentials, and native Swift classes to COM
implementations. The model preserves COM's binary calling convention, identity,
interface discovery, and reference-counting rules while presenting ordinary
Swift protocols, casts, and ARC operations in source.

COM interoperability is experimental. Enable it with
`-enable-experimental-com-interop`. A compilation may select the Microsoft
object model with `-com-interop-model=microsoft`.

The implementation separates language support from the selected COM model.  The
compiler owns the common type-system and ABI mechanisms. The supplemental `COM`
module defines model-specific identities, activation APIs, and the small set of
functions used by compiler-emitted vtables.

## Source model

### Interfaces

An `@com` protocol declares a COM interface. Its `interface:` argument is the
interface's stable 128-bit identity.

```swift
@com(interface: "17D6A539-7E13-4D88-8419-88CC2F3D15A1")
protocol IWidget {
  func GetValue(_ value: UnsafeMutablePointer<Int32>?) -> HRESULT
}
```

The identity is validated when the attribute is checked. It is stored in the
protocol descriptor in target-native GUID layout and is available as
`IWidget.IID` through the compiler-managed conformance of `IWidget.Type` to
`COMInterface`.

The metatype conformance is intentionally distinct from conformance by an
implementation type. Generic code that operates on interface declarations spells
the requirement as follows:

```swift
func __uuidof<Interface>(of: Interface.Type) -> IID
    where Interface.Type: COMInterface {
  Interface.IID
}
```

`COMInterface` is a compiler-managed, metatype-only protocol. It cannot be used
as an existential or explicitly adopted.

A COM interface requirement must have a C-representable ABI. Static, generic,
`async`, and `throws` requirements are not supported, nor are constructors or
associated types. Swift-only conveniences belong in an extension and do not
add vtable entries:

```swift
extension IWidget {
  var value: Int32 {
    get throws {
      var result: Int32 = 0
      let hr = GetValue(&result)
      guard SUCCEEDED(hr) else {
        throw COMError(hr: hr)
      }
      return result
    }
  }
}
```

A protocol-extension implementation may also satisfy a declared COM requirement
for a native Swift implementation. Its native COM method witness is serialized
just like a witness supplied by the class itself.

### Interface inheritance

Every interface declaration has one physical ABI chain. A derived interface may
refine no COM interface or may refine multiple declarations only when they are
all on the same refinement chain. Repeating comparable bases is harmless;
incomparable bases are rejected because they cannot describe one interface
pointer.

```swift
@com(interface: "10000000-0000-0000-0000-000000000001")
protocol IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000002")
protocol IDerived: IBase {
}

@com(interface: "10000000-0000-0000-0000-000000000003")
protocol IIndependent {
}

// Invalid: IBase and IIndependent require different interface pointers.
@com(interface: "10000000-0000-0000-0000-000000000004")
protocol IInvalid: IBase, IIndependent {
}
```

An interface may additionally refine marker protocols. A non-marker Swift
protocol cannot contribute requirements to a COM vtable. A protocol that refines
a COM interface must itself have an `@com(interface:)` identity, and identities
must be unique within an ABI chain.

COM conformances are class-only, even though a COM protocol does not need to
spell `AnyObject` as an inherited protocol. A conformance must be declared in
the implementation type's module and cannot be conditional.

### Implementations

A `@com` class is a native Swift class that exposes one or more COM interfaces:

```swift
@com
final class Widget: IDerived, IIndependent {
  func GetValue(_ value: UnsafeMutablePointer<Int32>?) -> HRESULT {
    value?.pointee = 42
    return S_OK
  }
}
```

`@com(implementation:)` additionally gives the class an activation identity:

```swift
@com(implementation: "5B071CDC-961D-4BEA-A74A-5D4A7F24BE5A")
public final class Widget: IDerived {
}
```

The class metatype then has a compiler-managed conformance to `COMActivatable`.
Under the Microsoft model its identity is exposed as `Widget.CLSID`. The 16
identity bytes are emitted once as a hidden, coalescable constant and are passed
directly as the metatype conformance witness. A generic class may participate in
COM but cannot have an activation identity.

`COMActivatable` is a model-independent concept. The spelling and use of its
identity are supplied by the selected object model; activation is not required
to use Microsoft's CLSID mechanism.

Under the Microsoft model, an activatable class may also declare its registry
threading model:

```swift
@com(implementation: "5B071CDC-961D-4BEA-A74A-5D4A7F24BE5A", threading: .free)
final class FreeThreadedWidget: IDerived {
}
```

The supported values are `.single`, `.apartment`, `.free`, `.both`, and
`.neutral`; `.apartment` is the default. This information describes activation
and registration. It does not add storage to an instance or change the COM
method calling convention.

Native Swift inheritance preserves inherited interface pointer positions. A
subclass may refine an inherited interface chain or append an independent chain.
An `open` implementation and its native COM method witnesses are serialized so a
client module can construct the correct vtables for a subclass. An inherited
native entry is reused unless the subclass overrides the corresponding
requirement.

Actors and classes using a non-native Swift object model cannot provide native
COM implementations.

### Object models

The selected object model supplies a logical root interface. The Microsoft model
supplies `IUnknown` as the root of every implementation. Another model, such as
one based on XPCOM, can instead supply its own root interface and runtime APIs
without changing the common language and ABI mechanisms.

Every native Swift COM implementation also exposes the compiler-managed
`ISwiftObject` interface. It allows a cast from a COM interface pointer back to
the native Swift heap object. Users cannot explicitly provide or replace this
conformance.

Source can select Microsoft-specific APIs with `#if $_MicrosoftCOM`. This
condition is enabled only when COM interoperability and the Microsoft model are
both enabled. It is part of a module's compilation identity and therefore
participates in module-cache and serialized-interface compatibility.

## COM existentials

### Representation and lifetime

An existential such as `any IWidget` is exactly one machine word containing an
interface pointer. It carries neither Swift type metadata nor a Swift protocol
witness table. The null pointer is the representation of an absent optional
interface.

Copying the existential calls `AddRef` through vtable slot 1. Destroying it
calls `Release` through vtable slot 2. Borrowing it performs neither operation.
The representation and operations are independent of whether the object was
implemented in Swift or by foreign code.

COM interface existentials are class-constrained by their conformance rules, but
they are not represented as native Swift `AnyObject` references. An existential
composition may contain one most-derived COM interface and any number of marker
protocols. It cannot combine unrelated COM interfaces, `AnyObject`, a superclass
constraint, or a non-marker protocol.

Independent interface constraints remain expressible in generic code:

```swift
func use<T: IReadable & IWritable>(_ value: borrowing T) {
  value.read()
  value.write()
}
```

This is a constraint on a native implementation, not a request to represent both
interfaces with one existential pointer.

COM existentials support ordinary storage, collection elements, closure
captures, `inout` access, and addressable member-access chains. `type(of:)`
produces the dynamic existential metatype without requiring native Swift object
metadata.

### Reference storage

Managed `weak` and checked `unowned` storage require a runtime weak-reference
facility. The base COM model does not define one, so these forms are rejected
for COM interface existentials. `unowned(unsafe)` is supported as a trivial,
non-owning interface pointer. Loading it into a strong value performs the
required `AddRef`.

Concrete native `@com` class references continue to use Swift's normal weak,
unowned, and unmanaged reference-storage operations because they point at the
Swift heap object rather than at a COM interface address.

Model-specific weak-reference protocols, such as the WinRT weak-reference
interfaces, can be layered on this base model in the future without changing the
one-word existential representation.

### C-representable parameters

A COM existential is C-representable as an interface pointer in a COM method
signature. A pointer to an optional existential represents an interface
out-parameter, so the following Swift code has the expected `IItem **` ABI:

```swift
@com(interface: "45000000-0000-0000-0000-000000000001")
protocol IProvider {
  func GetItem(_ item: UnsafeMutablePointer<(any IItem)?>?) -> HRESULT
}

var item: (any IItem)?
let hr = provider.GetItem(&item)
```

The address passed to the method is the address of the one-word optional
existential. A successful foreign call initializes it with an owned interface
reference, which ARC subsequently manages.

### Raw pointer ownership

Low-level APIs distinguish two conversions:

- Converting a borrowed interface to a raw pointer does not transfer ownership.
- Converting an owned `+1` raw interface pointer to a Swift existential adopts
  that reference without calling `AddRef`.

Creating an owned existential from a borrowed `+0` pointer must first acquire a
reference. `ManagedObject<Interface>` makes the adopting and borrowing forms
explicit without requiring `unsafeBitCast` in client code:

```swift
let widget = ManagedObject<IWidget>.takeRetainedValue(result)
let pointer = ManagedObject<IWidget>.passUnretained(widget)
```

`takeRetainedValue(_:)` consumes the ownership represented by a `+1` pointer
without calling `AddRef`. `passUnretained(_:)` returns the existing interface
pointer at `+0`. `ManagedObject` is a stateless conversion namespace; it adds no
wrapper allocation or storage and does not alter the existential or vtable ABI
described here.

## Dispatch and casting

Calling an interface requirement loads the target directly from the interface
vtable and performs an indirect call. Foreign objects therefore have the same
dispatch sequence as C or C++ and require no Swift adapter thunk.

A native Swift implementation has one COM ABI entry for each method witness.
The entry:

1. Uses the fixed adjustment stored immediately before the vtable address
   point to recover the native Swift heap object.
2. Adapts the COM ABI arguments to the Swift implementation's calling
   convention.
3. Tail-calls the Swift implementation when the target permits it.

The adjustment is constant for a given class and interface address point.

The operators `is`, `as?`, and `as!` use `QueryInterface` when the target is a
COM interface. A cast to a native Swift class first queries `ISwiftObject` and
then applies Swift's native type check. The common interface-cast path therefore
does not require Swift metadata in the source existential.

Under the Microsoft model, `===` and `ObjectIdentifier` follow COM identity
semantics. They query the canonical `IUnknown` pointer, so two distinct
interface pointers for the same object compare as identical.

Generic code constrained by a COM interface receives an interface-adjustment
witness rather than a Swift protocol witness table. It uses this witness to
project the native object to the required interface pointer. Refinements in one
ABI chain share an address point; independent interfaces have distinct
adjustments.

## Native object layout

A native Swift COM object remains a normal Swift heap object. Its allocation
has a compiler-generated prefix containing one vtable pointer for each
physical interface address point:

```text
lower addresses

  +-------------------------------+
  | lpVtbl for independent slot   |  <- interface pointer
  +-------------------------------+
  | lpVtbl for primary user slot  |  <- interface pointer
  +-------------------------------+
  | lpVtbl for ISwiftObject       |  <- interface pointer
  +-------------------------------+
  | native Swift heap object      |  <- Swift object pointer
  +-------------------------------+

higher addresses
```

The implementation's *interface frontier* is the set of most-derived,
independent interface chains it implements. Each frontier member receives one
physical address point. All logical interfaces in the same refinement chain
map to that address point. `ISwiftObject` occupies the stable slot closest to
the native object.

The allocator copies a constant prefix template into each allocation. The
per-instance cost is therefore one word per physical address point; the
vtables, interface map, and prefix template are read-only data shared by every
instance of the class.

### Vtable address point

An interface pointer loads `lpVtbl`, which points at callable slot 0. Two
implementation fields precede that public address point:

```text
lpVtbl[-2]  pointer to the class-wide interface map
lpVtbl[-1]  byte adjustment from this interface pointer to the Swift object
lpVtbl[ 0]  QueryInterface
lpVtbl[ 1]  AddRef
lpVtbl[ 2]  Release
lpVtbl[ 3]  first interface requirement
...         remaining requirements, base-most interface first
```

The negative slots are an implementation contract between Swift-emitted vtables
and the supplemental runtime functions. A foreign COM vtable has only the public
slots and is never inspected at negative indices.

An implementation conforming to `COMAggregatable` uses the aggregating forms
of `QueryInterface`, `AddRef`, and `Release` in slots 0 through 2. Aggregation
does not add a word to every object; the controller is supplied by the
conformance's borrowing `controller` property.

### Interface map

All native vtables for a class share one read-only interface map:

```c
struct Header {
  uint32_t count;
  uint32_t reserved;
};

struct Entry {
  int32_t descriptor;
  uint32_t index;
};
```

`descriptor` is a field-relative reference to the logical interface's protocol
descriptor. Its low bit indicates an indirect reference. `index` selects the
physical word in the object prefix. The map has one entry for every logical
interface in every frontier chain, including a root interface supplied by the
selected object model. `reserved` is currently zero and leaves room for
version or flag information without disturbing the eight-byte alignment of
entries.

`QueryInterface` first compares the requested IID with the IID trailing each
protocol descriptor in this map. It computes the result from the native object
pointer and the selected index. A `COMInterfaceResolver` may provide
conditional interfaces and tear-offs after this fixed-map fast path.

## Metadata and emitted identities

A COM protocol descriptor is marked as a special COM protocol and carries its
16-byte IID inline immediately after the fixed protocol-descriptor header. The
compiler-managed `COMInterface` metatype witness is the address of these bytes;
there is no second IID global or synthesized accessor.

An activatable class currently emits one target-native 16-byte identity named
with the `CLSID_` prefix followed by its nominal type descriptor mangling. It
has hidden, coalescable linkage so clients may materialize the class identity
without duplicating it in one linked image.

Native COM method witnesses are represented structurally in serialized SIL
witness tables. A method entry records both the ordinary Swift witness and its
COM ABI witness. This lets a client construct native vtables for public and open
classes without discovering functions from ad hoc symbol names or serializing
unrelated function bodies.

## Calling convention and mangling

SIL uses the `@convention(com_method)` function representation for COM method
entries and the `com_method` instruction for vtable lookup. The physical ABI is
the platform COM calling convention. On 32-bit x86 Windows this is `stdcall`;
on other supported targets it is the platform C calling convention.

The stable Swift mangling uses:

- `V` for the COM method function representation.
- `TWV` for a native COM method witness thunk. `TW` remains the ordinary Swift
  protocol witness thunk for the same conformance.

The compiler also gives private class-local data descriptive suffixes such as
`.com.interface_map`, `.com.vtable.<protocol>`, and `.com.prefix`. These names
are debugger aids for private implementation objects. They are guaranteed to
remain local and are not part of the public Swift decoration grammar or a client
ABI contract.

## Supplemental modules

The `COM` supplemental module contains the identity types, compiler-recognized
metatype protocols, activation support, aggregation hooks, and the C ABI
functions referenced by native vtables. The compiler validates only the minimum
runtime entries it needs and does not prescribe unrelated contents of the
module, allowing the library to evolve independently.

The `_COM_Concurrency` cross-import overlay adds asynchronous context helpers
when both `COM` and `_Concurrency` are imported. Core COM use does not require
Swift concurrency.

Under the Microsoft model, the module supplies `IUnknown`, CLSID-based
activation, apartment initialization, canonical identity comparison, and
aggregation support. These facilities are layered over the model-independent
interface, existential, dispatch, and native object-layout mechanisms.
