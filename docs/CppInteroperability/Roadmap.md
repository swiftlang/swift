# Swift and C++ Interoperability Roadmap

* Introduction
    * C++, as a language
    * C and Objective-C interoperability
* Specifics and trade-offs
* Goals
* The approach 
    * Foreign reference types
    * Owned types and value lifetimes
    * Iterators and ranges (by Egor)
    * Generic APIs https://forums.swift.org/t/bridging-c-templates-with-interop/55003
* Evolution and process
    * Example: virtual/overridden APIs
* How interop fits into the ecosystem more generally (lldb, ide, spm)
    * *The* Standard Library overlay (by Egor)

## Introduction

**This roadmap is** a sketch, rather than a final design for the “forward” (using C++ APIs from Swift) half of C++ and Swift interoperability. This roadmap outlines some high-level topics related to C++ interoperability, overarching goals that drive the project’s design decisions, a process for evolving C++ interoperability over time, and a collection of specific problems and their potential solutions. 

“Reverse” interoperability (using Swift APIs from C++) is another extremely important part of the interoperability story, however, reverse interoperability has largely different goals and constraints, which necessarily mean a different design and roadmap. Alex Lorenz will be putting together a roadmap for reverse interoperability which outlines these goals and will shed light onto the differences between the two halves of the projects. In the future this roadmap will need to be updated to explain how these two halves fit together and present the “bi-directional” interoperability story that is C++ and Swift interoperability as a whole. 

**C and Objective-C interoperability:** C++ interoperability will have to build off of the previous art of C and Objective-C interoperability, not only to preserve existing Swift codebases and provide a incremental story, but also to reduce unnecessary cognitivie burdon on developers. Starting with the name, C++ inherits much of C, which means C++ interoperability has a great starting place to build off of. C is a simple language without many high level constructs, so importing C APIs is, relatively, simple. On the other hand, Objective-C adds some high level constructs, such as reference types and a generics model. However, unlike C++, Objective-C has strong idioms; there are only so many patterns for doing some specific thing. So, while C and Objective-C interoperability is a good starting place, the model for importing these languages cannot be taken wholesale for C++ interoperability. 

## Goals

Safety is a top priority for the Swift programming language. Therefore, the Swift compiler should not import APIs that will likely be less safe in Swift than they are in C++ and interop should strive to make imported APIs *safer* in Swift than they are in C++ by providing safe API interfaces for common, unsafe C++ API patterns (such as iterators). This means that, while some common C++ APIs will be exposed through a safer interface in Swift, most C++ APIs will be imported with the same level of unsafety as they have in C++. 

In a similar vein, the Swift programming language strives to be fast. C++ interoperability should follow suite, making it a goal not to have hidden performance traps. C++ APIs should have similar performance in Swift as C++ and should be called directly when possible.

C++ is an un-opinionated language. There is little to no consensus on anything from naming, to generic programming, value categories, or error handling (to name a few things). This lack of opinion is in sharp contrast to Swift where there is generally one, specific way to do things. Even in the C++ Standard Library there is a rather un-standard set of API models. The un-opinionated nature of C++ means there are few, if any guarantees about a given C++ API, because the same functionality may be implemented using many different language features or programming paradigms. When consuming an arbitrary C++ API, you must be prepared for *anything*. Types with reference semantics (referred to as “reference types” in this document) are one example of this. Taking the reference type from our examples section:

```
// StatefulObject has object identity and reference semantcs: 
// it should be constructed with "create" and used via a pointer.
struct StatefulObject {
  StatefulObject(const StatefulObject&) = delete;
  StatefulObject() = delete;

  StatefulObject *create() { return new StatefulObject(); }
};
```

This type has specific reference semantics (described in the comment). The copy constructor is deleted, preventing some mis-use, but the semantics are largely not communicated. The C++ programmer is responsible for reading the comment to know how to use the type correctly; there is no concrete attribute that specifies `StatefulObject` should be used as a reference type. 

In Swift, reference types have their own language construct (`class`es). Swift clearly defines patterns for naming, generic programming, value categories, error handling, etc. and it is rare that a codebase deviates from the standard practice. Idiomatic, expressive Swift code uses these well-defined programming patterns, so, to align with the goals of the Swift programming language, and allow consumers of C++ APIs to have a similar experience, the Swift compiler should map C++ APIs to one of these specific patterns (editorial: re-word this crazy sentence to make it more clear/concise). To be able to map *any* C++ APIs to *some* specific Swift pattern, the Swift compiler must have some semantic information about the API. This semantic information may be provided through annotations or heuristics in the compiler. In the above example, the user should annotate `StatefulObject` as a reference type so that the Swift compiler can import it as a Swift `class`. 

To be able to follow the “Swift API model,” that is to have expressive APIs with strong idioms, the following goals are nessisary:

* C++ interoperability aims to provide a clear, well-defined mapping for APIs that can be imported, and to draw a clear boundary between these APIs and APIs that cannot be imported. A user should be able to read the C++ interoperability documentation and have a good idea of both how much of their C++ API will be able to be imported in Swift and what that API will look like once it is imported. 
* It is a goal to preserve the semantics (including performance semantics) that an API was written with by using information provided by the user to understand an API's semantics and mapping those semantics to a Swift idiom that represents the same thing.
* It is *not* a goal to import every C++ API into Swift. APIs that are imported by Swift should feel Swifty and idiomatic. There must be an easy way to add annotations so that APIs become importable, especially when updating APIs directly is not an option (because the source code is not available). 


Finally, Swift programmers have used C++ APIs since Swift 1.0 through an Objective-C or C bridging layer. It is important that programmers are able to *incrementally* remove these (potentially huge) bridging layers and start using their C++ APIs directly. To make this transition incremental, C++ interoperability must not change the way Swift imports any existing C or Objective-C APIs, even when C++ interoperability is enabled. 


## The approach

The approach for importing C++ APIs into Swift is derived from the fact that both our goals, necessitate that not all APIs are imported automatically; to import an API the user must provide semantic information in some cases. Further, in order to realize our goals (especially the goals of safety and clear, well defined mappings), it is impossible to import any arbitrary C++ API. APIs that are imported by Swift must fall into one of our understood categories or be annotated with semantic information that can be used to classify them into a Swift idiom. 

In this section, the roadmap will discuss several of the most common C++ API patterns and how they might be imported by the Swift compiler using the goals outlined above.

### Reference types

Swift types fall into two categories: value types and reference types. The same is true for Objective-C types, which makes importing types easy: structs are imported as value types and Objective-C classes are imported as Swift class types. The same is not true for C++. In typical C++ fashion, there is no clear idiom for defining reference types. All types in C++ are declared in the same way, but some types have reference semantics and others don’t. To be able to express reference types in Swift natively, users must annotate their reference types as such, which will tell the compiler to import them as Swift classes (which have the same semantics). 

When importing a reference type, there are three main sub-categories that reference type can fall into: 1) **immortal** reference types are “intentionally leaked” and live for the duration of the program 2) **manually managed** reference types are constructed and destroyed manually by the programmer, these are unsafe reference types and should maybe be renamed accordingly 3) **automatically managed** reference types provide retain and release operations which are used to safely destroy a value of the type after its last use. [(Examples below.)](## Examples and Definitions)

Reference types, their instances, methods on reference types, and other APIs that use reference types generally fit well into the existing Swift model and their use should be allowed without restriction. While it is possible to define unsafe APIs that use reference types, these APIs will not be any less safe than their Swift or C++ counterpart, so there is no reason to dis-allow them. Note: the only case where C++ APIs using reference types must be dis-allowed is when there is not one level of indirection provided (either a reference or pointer). In this case, the C++ API is not using the type as a reference types, and thus breaks the reference type definition (above).

As per our goal’s specification, this method of importing reference types allows C++ interoperability to have a clear, native mapping for a common C++ API pattern that builds a more general purpose solution off of the pre-established reference type bridging from Objective-C. Additionally, Swift preserves the same safety properties as C++ while providing an even safer option for the common pattern where reference types have a retain and release operation.

### Value types

As mentioned above, swift types fall into two categories: value types and reference types. Value types are relatively simple in Swift, C, and Objective-C, however, the same is not true for C++ where value types may have complex lifetime operations that are used to express non-native concepts (to Swift) such as value types that own memory. This case of owned value types is specifically difficult to map on to Swift as it is novel to the language and does not always work well with the existing model for Swift value types that are trivially copyable.  

#### Trivial types

This roadmap will refer to trivial value types that do not hold pointers as “trivial types.” These types include primitive types such as integers and types which are composed of other trivial types. Trivial types are “owned” types that provide trivial lifetime operations: a copy is a copy of their bits and a destroy is a no-op. Trivial types have roughly the same mapping throughout Swift, C, Objective-C, and C++ making them trivial to import. Trivial types, their instances, methods on trivial types, and other APIs that use trivial types are generally considered to be safe and usable.

**Pointer types**

This roadmap will refer to trivial value types that hold pointers as “pointer types.” These types include pointers themselves and types which are composed of any pointer types (potentially including other types as well). The pointers held by pointer types refer to memory that is *not owned* by the pointer type (making pointer types a “view” or “projection” into memory). While pointer types are very similar to trivial types with respect to their lifetime operations and the fact that they map similarly in these four language, they differ in the fact that while they themselves are not inherently unsafe, they may be used in unsafe APIs (discussed later).

#### Owned types

This category of types refers to types that own memory. These types are pointer types where the type *does own* the memory that their pointers point to. C++ often uses copy constructors and destructors to manage the lifetime of owned types. Therefore, the Swift compiler should assume that pointer types with custom copy constructor and destructors own their memory (and should provide annotations to override this assumption).

#### Lifetime and safety of owned types

Value types that own memory do not natively exist in Swift today. Because Swift was not built around this kind of value type, Swift handles the lifetime of value types in a subtly different way from C++. This subtle difference makes a naive mapping of C++ value types that own memory to native Swift value types extremely dangerous, especially when dealing with projections of owned storage. Let’s look at an example Swift program that naively imports some owned type and returns a projections of it: 

```
var v = vector(1) 
let start = v.begin() 
doSomething(start) 
fixLifetime(v)
```

Here, because Swift inserts a copy before the call to begin, v projects a dangling reference. This is an example of how subtly different lifetime models make using C++ types from Swift hard, if their semantics aren’t understood by the compiler. 

To make these APIs safe and usable, Swift cannot import unsafe projections of types that own memory, because they don’t fit the Swift model. Instead, the Swift compiler can try to infer what, semantically, the API is trying to do, or the library author can provide this information via annotations. In this case, the Swift compiler can infer that begin returns an iterator, which Swift can represent through the existing, safe Swift iterator interface. In the future, Swift may gain the necessary ownership features to import and use projections of types that own memory in a safe way, but today, this is not possible, and importing these projections would result in a *less safe* API in Swift than in C++ (violating the first goal above). Other than methods which project owned memory, owned types, their instances, methods on owned types, and other APIs that use owned types are generally considered to be safe and usable.

Note: in the example above, “start” is a pointer type. The use of this pointer in the “begin” API is unsafe, but the type of start itself is not unsafe. In other words, safety restrictions need not be applied to pointer types themselves but rather their unsafe uses.

Editorial note: the following three paragraphs have a lot of overlap with the above section and should be folded together somehow. 

C++ often projects the storage of owned types. C++ is able to tie the lifetime of the projection to the source using lexcal scopes. Because there is a well-defined, lexical point in which objects are destroyed, C++ users can reason about projection’s lifetimes. While these safety properties are less formal than Swift, they are safety properties none-the-less, and they are a model that works in C++. 

This model cannot be adopted in Swift, however, because the the same lexical lifetime model does not exist. Further, projections of owned types are completely foreign concept in Swift, meaning users aren’t used to programming in terms of this model, and may not be aware of the added (implicit) constraints (that is, when objects are destroyed). Swift’s language model is such that returning projections from a copied value, even in smaller lexical scope, should be safe. In order to allow projections of owned types, this assumption must be broken, or C++ interoperability must take advantage of Swift ownership features to associate the lifetime of the projection to the source. 

The following example highlights the case described above:

```
func getCString(str: std.string) -> UnsafePointer<CChar> { str.c_str() }
```

The above function returns a dangling reference to `str`‘s inner storage. In C++, it is assumed that the programmer understands this is a bug, and generally would be expected to take `str` by reference. This is not the case in Swift. To represent this idiomatically in Swift, the lifetimes must be associated through a projection. Using the tools provided in the ownership manifesto this would mean yielding the value returned by `c_str` out of a [generalized accessor](https://github.com/apple/swift/blob/main/docs/OwnershipManifesto.md#generalized-accessors)(resulting in an error when the pointer is returned). 
 

### Iterators

Because iterators and ranges follow some specific API semantics, the Swift compiler can map them to a safe, ergonomic, native interface which Egor Zhdan will describe here :)

### Generic APIs

C++ provides a couple of tools for writing generic APIs: templates, concepts, virtual classes (inheritance), and various combinations and permutations of these. Templates are likely the most common tool for creating generic APIs. Unfortunately, as outlined in [this forum post](https://forums.swift.org/t/bridging-c-templates-with-interop/55003) (Bridging C++ Templates with Interop), C++ templates do not map cleanly to Swift generics, making interoperability between these generic APIs extremely difficult. Despite this, the linked forum post proposes various strategies for importing C++ templates derived from goals similar to the ones outlines in this roadmap. These proposals should probably be factored into this section of the roadmap at some point in the future.

## Evolution process

Several specific API patterns are outlined above. These specific API patterns will each need a detailed, self-contained, evolution proposal which can take context from and be framed by this roadmap. Once each of these specific API patterns is accepted by the Swift community (through the evolution process) the design will be ratified and that part of Swift and C++ interoperability will no longer be considered experimental.

As discussed many time before, the C++ language is huge and scattered (refrase). This roadmap allows specific, focused, and self contained evolution proposals to be created for individual pieces of the language and specific programming patterns by providing goals that lend themself to this kind of incremental design and evolution (by not importing everything and requiring specific mappings for specific API patterns) and by framing interop in a larger context that these individual evolution proposals can fit into.

For example, “virtual type interfaces that API consumers provide implementations for” are a fairly common API pattern in C++. Currently, there is no design for such a pattern, but some Swift contributor may extend interop to provide a clear, native mapping for this API pattern through a self-contained proposal following the goals outlined by this document, and framed by “the approach” described above. 

## The Swift ecosystem

### Tooling and build process

It goes without saying (yet will be said anyway) that as a supported language feature, C++ and Swift interoperability must work well on every platform supported by Swift. In a similar vein, tools in the Swift ecosystem should be updated to support interoperability features. For example, SourceKit should provide autocompletion, jump-to-definition, etc. for C++ functions, methods, and types and lldb should be able to print C++ types (even in Swift frames). Finally, the Swift package manager should be updated with the necessary features to support building C++ dependencies, a topic which Saleem Abdulrasool may be able to expand on here :)

This roadmap outlines a strategy for importing APIs that relies on semantic information from the user. In order to make this painless for users across a variety of projects, Swift will need to provide both inline annotation support for C++ APIs and side-file support for APIs that cannot be updated. For Objective-C, this side-file is an APINotes file. As part of Swift and C++ interoperability, APINotes will either need to be updated to support C++ APIs, or another kind side-file will need to be created. 

### The standard library

Egor Zhdan will discuss importing the standard library, the Swift C++ standard library overlay, etc. :)
* * *

## Examples and Definitions

*Reference Types* 

Reference types have reference semantics and object identity. A reference type is a pointer (or “reference”) to some object which means there is a layer of indirection. When a reference type is copied, the pointer’s value is copied rather than the object’s storage. This means reference types can be used to represent non-copyable types in C++.

*Manually Managed Reference Types*

Here a programmer has written a very large `StatefulObject` which contains many fields:

```
struct StatefulObject {
  std::array<std::string, 32> names;
  std::array<std::string, 32> places;
  // ...
  
  StatefulObject(const StatefulObject&) = delete;
  StatefulObject() = delete;

  StatefulObject *create() { return new StatefulObject(); }
};
```


Because this object is so expensive to copy, the programmer decided to delete the copy constructor. The programmer also decided that this object should be allocated on the heap, so they decided to delete the default constructor, and provide a create method in its place. 

In Swift, this `StatefulObject` should be imported as a reference type, as it has reference semantics.

*API Incorrectly Using Reference Types*

Here someone has written an API that uses `StatefulObject` as a value type.

```
StatefulObject makeAppState();
```

This will invoke a copy of `StatefulObject` which violates the semantics that the API was written with. To be useable from Swift, this API needs to be updated to pass the object indirectly (by reference):

```
StatefulObject *makeAppState(); // OK
const StatefulObject *makeAppState(); // OK
StatefulObject &makeAppState(); // OK
const StatefulObject &makeAppState(); // OK
```

*Immortal Reference Types*

Instances of StatefulObject above are manually managed by the programmer, they create it with the create method and are responsible for destroying it once it is no longer needed. However, some reference types need to exist for the duration of the program, these references types are known as “immortal.” Examples of these immortal reference types might be pool allocators or app contexts. Let’s look at a GameContext object which allocates (and owns) various game elements:

```
struct GameContext {
  // ...
  
  GameContext(const GameContext&) = delete;

  Player *createPlayer();
  Scene  *createScene();
  Camera *createCamera();
};
```

Here the GameContext is meant to last for the entire game as a global allocator/state. Because the context will never be deallocated, it is known as an “immortal reference type” and the Swift compiler can make certain assumptions about it. 

*Automatically Managed Reference Types*

While the `GameContext` will live for the duration of the program, individual `GameObject` should be released once they’re done being used. One such object is Player:

```
struct GameObject {
  int referenceCount;
  
  GameObject(const GameObject&) = delete;
};

void gameObjectRetain(GameObject *obj);
void gameObjectRelease(GameObject *obj);

struct Player : GameObject {
  // ...
};
```

Here Player uses the `gameObjectRetain` and `gameObjectRelease` function to manually manage its reference count in C++. Once the `referenceCount` hits `0`, the Player will be destroyed. Manually managing the reference count is prone to errors, as programmers may forget to retain or release the object. Fortunately, this kind of reference counting is something that Swift is very good at. To enable automatic reference counting, the user can specify the retain and release operations via attributes directly on the `GameObject`. This means the programmer no longer needs to manually call `gameObjectRetain` and `gameObjectRelease`; Swift will do this for them. They will also benefit from the suite of ARC optimizations that Swift has built up over the years. 

*Owned types*

Owned types “own” some storage which can be copied and destroyed. An owned type must be copyable and destructible. The copy constructor must copy any storage that is owned by the type and the destructor must destroy that storage. Copies and destroys must balance out and these operations must not have side effects. Examples of owned types include `std::vector` and `std::string`.

*Trivial types*

Trivial types are a subset of owned types. They can be copied by copying the bits of a value of the trivial type and do not need any special destruction logic. Examples of trivial types are `std::array` and `std::pair<int, int>`. 

*Pointer Types*

Pointer types are trivial types that hold pointers or references to some un-owned storage (storage that is not destroyed when the object is destroyed). Pointer types are not a subset of trivial types or owned types. Examples of pointer types include `std::string_view` and `std::span` and raw pointer types such as `int *` or `void *`.

*Projections*

Projections are values rather than types. An example of a method which yields a projection is the `c_str` method on `std::string`.

```
struct string { // String is an owned type.
  char *storage;
  size_t size;
  
  char *c_str() { return storage; } // Projects internal storage
```

Iterators are also projections:

```
  char *begin() { return storage; } // Projects internal storage
  char *end() { return storage + size; } // Projects internal storage
```

Because `string` is an owned type, the Swift compiler cannot represent a projection of its storage, so the `begin`, `end`, and `c_str` APIs are not imported.
