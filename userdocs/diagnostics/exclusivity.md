# Exclusive Access to Memory

Swift's **Law of Exclusivity** states that two accesses to the same mutable memory aren't allowed to overlap in time _unless_ both accesses are reads (as opposed to writes) or both accesses are atomic.

In many cases, overlapping accesses aren't a problem because many are instantaneous and, by definition, cannot overlap with each other. In other words, from the perspective of the current thread the access begins and ends without any other code being able to interfere. One example of an instantaneous access is assigning to a stored property. However, some operations, like calling a `mutating` method on a stored property, are non-instantaneous. In this case, the write access to the stored property will last until the method returns. If the body of the method contains another access to the property, an exclusivity violation will occur.

Some exclusivity violations can be flagged at compile time and will be reported as errors. However, others can only be detected at runtime. The Swift runtime will dynamically enforce exclusivity and trap if it encounters a violation.

To see more examples and learn more about ensuring exclusive access to memory, see the [Memory Safety](https://docs.swift.org/swift-book/LanguageGuide/MemorySafety.html) chapter of _The Swift Programming Language_.