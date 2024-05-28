# Embedded Swift -- ABI

**⚠️ Embedded Swift is experimental. This document might be out of date with latest development.**

**‼️ Use the latest downloadable 'Trunk Development' snapshot from swift.org to use Embedded Swift. Public releases of Swift do not yet support Embedded Swift.**

For an introduction and motivation into Embedded Swift, please see "[A Vision for Embedded Swift](https://github.com/apple/swift-evolution/blob/main/visions/embedded-swift.md)", a Swift Evolution document highlighting the main goals and approaches.

## Calling convention of Embedded Swift

As of today, Embedded Swift has identical calling convention to full Swift. However, this does not need to continue in the future, and there should not be expectations that the ABI of Embedded Swift is compatible with full Swift.

## Metadata ABI of Embedded Swift

Embedded Swift eliminates almost all metadata compared to full Swift. However, class metadata is still used, because those serve as vtables for dynamic dispatch of methods to implement runtime polymorphism. The layout of Embedded Swift's class metadata is *different* from full Swift:

- The **super pointer** pointing to the class metadata record for the superclass is stored at **offset 0**. If the class is a root class, it is null.
- The **destructor pointer** is stored at **offset 1**. This function is invoked by Swift's deallocator when the class instance is destroyed.
- The **ivar destroyer** is stored at **offset 2**. This function is invoked to destroy instance members when creation of the object is cancelled (e.g. in a failable initializer).
- Lastly, the **vtable** is stored at **offset 3**: For each Swift class in the class's inheritance hierarchy, in order starting
  from the root class and working down to the most derived class, the function pointers to the implementation of every method of the class in declaration order in stored.

## Heap object layout in Embedded Swift

Heap objects have the following layout in Embedded Swift:

- The **isa pointer** (pointer to the class metadata) is stored at **offset 0**.
- The **refcount** is stored inline at **offset 1**.
- Normal stored properties follow.
