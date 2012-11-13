Swift Intermediate Language
===========================

Abstract
--------

TODO: enable high-level optimization, interoperability, persistence, embedded interpreter, second coming, etc.

General features
----------------

TODO: SSA, functional representation, etc.

Types
-----

SIL's type system is Swift's with some additional aspects. Like Swift, there are two
broad categories of types based on value semantics:

* *reference types*, which are handles to reference-counted boxes and are stored and passed around
  by reference, for example, classes and functions.
* *value types*, which are stored in-line and passed by value, for example, structs, tuples, and
  primitive types.

SIL classifies types into two additional subgroups based on ABI stability:

* *loadable types* are types with a fully exposed concrete representation. All
  reference types are loadable. Value types are loadable if they are fragile and all of their
  component types are fragile.
* *address-only types* are value types for which the compiler cannot access a full concrete
  representation, such as resilient value types or types that contain resilient types. Memory that
  contains an address-only type may be referred to by an address, but those addresses cannot be
  directly loaded or stored.

SIL adds some additional types of its own:

* The *address of T* ``Address<T>``, a pointer to memory containing a value of any reference or
  value type ``T``.  This can be an internal pointer into a data structure. Addresses of loadable
  types can be loaded and stored to access values of those types. Addresses cannot be retained or
  released.
* The *object pointer* ``ObjectPointer`` is a generic pointer to a retainable block of memory, or
  *box*. An object pointer behaves like a reference type, but its contents are not accessible; it
  can only be retained, released, or passed around. Operations that allocate retainable memory
  generally return both an object pointer for the box and an address pointing to the object or
  objects inside the box.

Instructions
------------

Memory Management
~~~~~~~~~~~~~~~~~

TODO: formalize SIL source notation

alloc_var
`````````
::

  %1:Address<T> = alloc_var {heap|stack} T

Allocates enough uninitialized memory to contain a value of type ``T``, either from the heap or from
the stack. The result of the instruction is the address of the allocated memory. The memory must
be deallocated with a ``dealloc`` instruction of the matching ``heap`` or ``stack`` type. The memory
will not be retainable; to allocate a retainable box for a value type, use ``alloc_box``.

alloc_ref
`````````
::

  %1:T = alloc_ref T

Allocates an object of reference type ``T``. The object will be initialized with retain count 1; it
will be uninitialized otherwise.

alloc_tmp
`````````
XXX does this still need to be different from alloc_var?

alloc_box
`````````
::

  %1:(ObjectPointer,Address<T>) = alloc_box T

Allocates a box large enough to hold a value of type ``T``. The result of the instruction is a pair
containing an object pointer to the box as its first element and an address of type ``T`` pointing
to the storage for the value inside the box as its second. The box will be initialized with a
retain count of 1; the storage will be uninitialized.

alloc_array
```````````
::

  %1:(ObjectPointer,Address<T>) = alloc_array T, %0:Int

Allocates a box large enough to hold an array of ``%0`` values of type ``T``. The result of the
instruction is a pair containing an object pointer to the box as its first element and an address
of type ``T`` pointing to the storage for the first element of the array inside the box as its
second. The box will be initialized with a retain count of 1; the storage will be uninitialized.

dealloc
```````
::

  dealloc {heap|stack} %0:Address<T>

Deallocates memory previously allocated by ``alloc_var``. The value in memory must be released
released prior to being deallocated.

retain
``````
::

  retain %0:T

Retains the value represented by ``%0``. If it is of a value type, this is a no-op. If the
value is of a reference type or is an object pointer, the retain count of the referenced box is
increased by one. Retaining an address is an error.

release
```````
::

  release %0:T

Releases the value represented by ``%0``. If it is of a value type, this destroys the value. If
the value is of a reference type or is an object pointer, the retain count of the referenced box is
decreased by one, and if it becomes zero, the referenced object is destroyed and the memory is
deallocated. The released value is invalid after the release instruction is executed. Releasing an
address is an error.

TODO: does releasing a value type really destroy it? should destroying a value type be a separate op?

destroy
```````
::

  destroy %0:Address<T>

Releases the value in memory at address ``%0``. This is equivalent to::

  %1 = load %0
  release %1
except that ``destroy`` must be used if ``T`` is an address-only type. This only releases the
referenced value; the memory may additionally need to be deallocated with a separate ``dealloc``
instruction.

load
````
::

  %1:T = load [take] %0:Address<T>

Loads the value at address ``%0`` from memory. ``T`` must be a loadable type. A load with the
``take`` attribute will destroy the value in memory (but not deallocate the memory) in the process
of the load; without the attribute, the load will ensure the value in memory remains valid.

store
`````
::

  store %0:T -> [initialize] %1:Address<T>

Stores the value ``%0`` to memory at address ``%1``. ``T`` must be a loadable type. A store with the
``initialize`` attribute will initialize uninitialized memory. A store without the attribute will
reassign memory with a live value already present.

copy
````
::

  copy [take] %0:Address<T> -> [initialize] %1:Address<T>

Loads the value at address ``%0`` from memory and stores it back into memory at address ``%1``. This
is equivalent to::

  %2 = load %0
  store %2 -> %1
except that ``copy`` must be used if ``T`` is an address-only type. The operands of ``copy`` may
be given the ``take`` and ``initialize`` attributes to indicate respectively whether the source may be
destroyed and whether the destination must be initialized.

TODO other instructions
~~~~~~~~~~~~~~~~~~~~~~~

Examples
--------

TODO
