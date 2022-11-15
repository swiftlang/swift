# Using Swift from C++

This vision document presents a high level overview of the "reverse" (i.e. Swift-to-C++) half of the C++ interoperability Swift language feature. It highlights the key principles and goals that determine how Swift APIs are exposed to C++ users. It also outlines the evolution process for this feature. This document does not present the final design for how Swift APIs get mapped to C++ language constructs. The final design of this feature will evolve as this feature goes through the Swift evolution process. This document does not cover the "forward" (i.e. using C++ APIs from Swift) aspect of the C++ interoperability, as it’s covered by a [sibling document](https://github.com/apple/swift/pull/60501/files).

This document is a prospective feature vision document, as described in the [draft review management guidelines](https://github.com/rjmccall/swift-evolution/blob/057b2383102f34c3d0f5b257f82bba0f5b94683d/review_management.md#future-directions-and-roadmaps) of the Swift evolution process. It has not yet been approved by the Language Workgroup.

## Introduction

Swift currently provides support for interoperability with C and Objective-C. Swift can import C and Objective-C APIs, and it can also expose its `@objc` types and functions to Objective-C. However, Swift currently does not provide support for interoperability with C++. Supporting bidirectional C++ interoperability in Swift would allow it to call into C++ APIs, and would make Swift APIs accessible to C++. Making Swift APIs accessible to C++ would simplify the process of adoption of Swift in existing C++ codebases, as it would allow them to incrementally adopt Swift. Furthermore, it would make Swift-only libraries available to C++ codebases.

## Overview of Swift's interoperability support

Swift uses two very different approaches for interoperability with C and Objective-C. For "forward" interoperability, Swift embeds a copy of the Clang compiler, which it uses to directly load C and Objective-C Clang modules and translate declarations into a native Swift representation. For "reverse" interoperability, Swift does not want to require Clang to embed Swift, and so Swift generates an Objective-C header that can be used to call into Swift APIs that are exposed in that header. Because Objective-C is restricted in what kinds of types it can work with, and because it requires code to be emitted in a special way that doesn't match the regular Swift ABI, methods and types must be marked as being exposed to Objective-C with the `@objc` attribute.

Swift's support for C++ interoperability is modeled after its support for C and Objective-C interoperability. For "forward" interoperability, the embedded Clang compiler is used to directly load C++ Clang modules and translate declarations into a native Swift representation. The "forward" interoperability [vision document](https://github.com/apple/swift/pull/60501/files) provides more details about this model. For "reverse" interoperability, Swift generates a header that uses C++ language constructs to represent Swift APIs that are exposed by the Swift module. Because C++ is much more expressive and flexible than Objective-C, the generated header is able to provide representation for native Swift functions, methods, initializers, accessors and types. This allows C++ programmers to call into Swift APIs using the familiar C++ function and member function call syntax. The ABI of a Swift declaration does not change when it gets exposed to C++, and thus Swift APIs do not require additional ABI annotations for C++ interoperability. This makes it possible to expose all supported APIs in a public interface of a Swift module.

## Goals

Swift and C++ are both feature rich programming languages. They share many similar features, but Swift has some features that lack close analogues in C++. The primary goal of Swift-to-C++ interoperability is to expose every language feature of Swift for which a reasonable C++ API can be created to represent that feature. For example, C++ does not have a concept of argument labels, but the argument labels can be integrated into the base name of a function to get a roughly similar effect. On the other hand, C++ does not have any feature that can represent a Swift feature like result builders in a reasonable manner. Therefore, APIs that use Swift features like result builders must be dropped in the C++ interface to the module.

It is a goal that Swift-to-C++ interoperability can be used by both mixed Swift/C++ language projects that want to interoperate within themselves, and by C++ projects that need to use a Swift library that wasn't developed with C++ interoperability in mind.

It is a goal of Swift-to-C++ interoperability to expose Swift APIs in a safe, performant and ergonomic manner. The Swift compiler and the language specification should follow several key related principles that are presented below.

### Safety

Safety is a top priority for the Swift programming language. Swift code expects its callers to adhere to Swift’s type rules and Swift’s memory model, regardless of whether it’s called from Swift or C++. Thus, the C++ code that calls Swift should properly enforce Swift’s expected language contracts. The enforcement should be done automatically in the generated header. This kind of enforcement does not prevent all possible issues though, as it does not change C++'s safety model. C++ is unsafe by default, and the user is able to use regular C++ pointers to write to memory as if they were using Swift's `UnsafeMutablePointer` type. This means that bugs in C++ code can easily lead to violations of Swift's invariants. For instance, a bug in user’s C++ code could accidentally overwrite a Swift object stored on the heap, which could cause unexpected behavior (such as a segfault) in Swift code. The user is expected to obey Swift's memory model and type rules when calling into Swift from C++, and thus they bear the ultimate responsibility for avoiding bugs like this one. The user can use certain program analysis tools, such as Address Sanitizer, to help them catch bugs that violate Swift's memory model rules.

Swift expects that values of correct types are passed to Swift APIs. For instance, for calls to generic functions, Swift expects the caller to pass a value of type that conforms to all of the required generic requirements. Type safety should be enforced from C++ as well. The C++ compiler should verify that correct types are passed to the Swift APIs as they’re invoked from C++. A program that tries to pass an incorrect Swift type into a Swift function should not compile. Select uses of specific Swift types might be incompatible with static type validation. For example, verification of generic requirements for a Swift opaque type value returned from a Swift function in C++ requires run-time validation. The program should report a fatal error when such run-time type validation fails, to ensure that the type invariants that Swift expects are not violated. The reported fatal error should clearly indicate why type validation failed and point to the location in the user's code which caused the run-time check.

Memory safety is paramount for Swift code. Swift automatically manages the lifetime of value and reference types in Swift. These principles should translate to C++ as well. The lifetime of Swift values that are created or passed around in C++ should be managed automatically. For instance, the generated C++ code should increment the retain count of a Swift class instance when a Swift class type value is copied in C++, and it should decrement the retain count of a Swift class instance when a Swift class type value is destroyed in C++. The default convention for using Swift types in C++ should discourage dangling references and any other patterns that could lead to an invalid use of a Swift value.

Swift’s memory safety model also requires exclusive access to a value in order to modify that value. For instance, the same value can not be passed to two `inout` parameters in the same function call. Swift enforces exclusivity using both compile-time and run-time checks. The generated run-time checks trap when an exclusivity violation is detected at runtime. Calls into Swift APIs from C++ should verify exclusivity for Swift values as well.

### Performance

Swift-to-C++ bridging should be as efficient as possible. The Swift compiler should avoid unnecessary overhead for calling into Swift functions from C++, or using Swift types from C++. Generally, the bridging code should not convert Swift types into their C++ counterparts automatically, as that can add a lot of overhead. For instance, a Swift function returning a `String` should still return a Swift `String` in C++, and not a `std::string`. However, some specific Swift "primitive" types should be converted into their C++ counterparts automatically in order to improve the ergonomics of how they are used in C++. The conversion for such primitive types should be zero-cost as their layout and size should match between Swift and C++. For instance, a Swift function returning a `Bool` can return a `bool` in C++ as they use the same underlying primitive LLVM type to represent the boolean value.

Some Swift features require additional overhead to be used in C++. Resilient value types are a good example of this; C++ expects types to have a statically-known layout, but Swift's resilient value types do not satisfy this, and so the generated C++ types representing those types may need to dynamically allocate memory internally. In cases like these, the C++ interface should at least strive to minimize the dynamic overhead, for example by avoiding allocations for sufficiently small types.

### Achieving safety with performance in mind

Certain aspects of Swift’s memory model impose certain restrictions that create tension between the goal of achieving safety and the goal of avoiding unnecessary overhead for calling into Swift from C++. Checking for exclusivity violations is a good example of this. The C++ compiler does not have a notion of exclusivity it can verify, so it is difficult to prove that a value is accessed exclusively in the C++ code that calls into Swift. This means that the C++ code that calls into Swift APIs will most likely require more run-time checks to validate exclusivity than similar Swift code that calls the same Swift APIs.

The adherence to Swift’s type and memory safety rules should be prioritized ahead of performance when C++ calls into Swift, even if this means more run-time checks are required. For users seeking maximum performance, Swift provides additional compiler flags that avoid certain run-time checks. Those flags should be taken into account when the C++ header is generated. An example of such flag is `-enforce-exclusivity`. When `-enforce-exclusivity=none` is passed to Swift, the Swift compiler does not emit any run-time checks that check for exclusivity violations. A flag like this should also affect the generated C++ header, and the Swift compiler should not emit any run-time checks for exclusivity in the generated C++ header when this flag is used.

### Ergonomics

Swift APIs should be mapped over to C++ language features that have a direct correspondence to the Swift language feature. In cases where a direct correspondence does not exist, the Swift compiler should provide a reasonable approximation to the original Swift language feature using other C++ constructs. For example, Swift’s enum type can contain methods and nested types, and such constructs can’t be represented by a single C++ enum type in an idiomatic manner. Swift’s enum type can be mapped to a C++ class instead that allows both enum-like `switch` statement behavior and also enables the C++ user to invoke member functions on the Swift enum value and access its nested types from C++.

The C++ representation of certain Swift types should be appropriately enhanced to allow them to be used in an idiomatic manner. For instance, it should be possible to use Swift’s `Array` type (or any type that conforms to `Collection`) in a ranged-based for loop in C++. Such enhancements should be done with safety in mind, to ensure that Swift’s memory model is not violated.

There should be no differences on the C++ side between using libraries that opt-in into library evolution and libraries that don’t, except in specific required cases, like checking the unknown default case of a resilient enum.

### Clear language mapping rules

C++ is a very expressive language and it can provide representation for a lot of Swift language constructs. Not every Swift language construct will map to its direct C++ counterpart. For instance, Swift initializers might get bridged to static `init` member functions instead of constructors in C++, to allow C++ code to call failable initializers in a way that’s consistent with other initializer calls. Therefore, it’s important to provide documentation that describes how Swift language constructs get mapped to C++. It is a goal of C++ interoperability to provide a clear and well-defined mapping for how Swift language constructs are mapped to C++. Additionally, it is also a goal to clearly document which language constructs are not bridged to C++. In addition to documentation, compiler diagnostics should inform the user about types or functions that can’t be exposed to C++, when the user wants to expose them explicitly. It is a goal of C++ interoperability to add a set of clear diagnostics that let the user know when a certain Swift declaration is not exposed. It is not a goal to diagnose such cases when the user did not instruct the compiler to expose a declaration explicitly. For example, the Swift compiler might not diagnose when an exposed Swift type does not expose one of its public methods to C++ due to its return type not being exposed, if such method does not have an explicit annotation that instructs the compiler that this method must be exposed to C++.

Some Swift APIs patterns will map to distinct C++ language constructs or patterns. For instance, an empty Swift enum with static members is commonly used in a namespace-like manner in Swift. This kind of enum can be mapped to a C++ namespace. It is a goal of C++ interoperability to provide a clear mapping for how Swift API patterns like this one are bridged to C++.

The semantics of how Swift types behave should be preserved when they’re mapped to C++. For instance, in C++, there should still be a semantic difference between Swift value and reference types. Additionally, Swift’s copy-on-write data types like `Array` should still obey the copy-on-write semantics in C++.

### Swift API design should be unaffected

Swift API authors should not change the way they write Swift code and design Swift APIs based on how specific Swift language constructs are exposed to C++. They should use the most effective Swift constructs for the task. It is a key goal of C++ interoperability that the exposed C++ interfaces are safe, performant, and ergonomic enough that programmers will not be tempted to make their Swift code worse just to make the C++ interfaces better.

### Objective-C support

The existing Swift to Objective-C bridging layer should still be supported even when C++ bindings are generated in the generated header. Furthermore, the generated C++ bindings should use appropriate Objective-C++ types or constructs in the generated C++ declarations where it makes sense to do so. For instance, a Swift function that returns an Objective-C class instance should be usable from an Objective-C++ compilation unit.

## The approach

The Swift compiler exposes Swift APIs to C++ by generating a header file that contains C++ declarations that wrap around the underlying calls into Swift functions that use the native Swift calling convention. The header also provides a suitable representation for Swift types. Typically, a single header file is generated for one Swift module.

Currently the generated header file depends on several LLVM and Clang compiler features for the Swift calling convention support, and thus it can only be compiled by Clang. The header does not depend on any other Clang-specific C++ language extensions. The header might depend on some other Clang-only features that improve developer experience for the C++ users, like specific attributes that improve diagnostics.

The generated header file uses advanced C++ features that require a recent C++ language standard. C++20 is the recommended language standard to use for Swift APIs, however, the generated header should also be compatible with C++17 and C++14.

The generated header file also contains the C and the Objective-C declarations that Swift exposes to C and Objective-C on platforms that support C or Objective-C interoperability. Thus a single header file can be used by codebases that mix C, Objective-C and C++.

The generated header file is a temporary build artifact. On platforms with ABI stability, the generated C++ code for an ABI stable Swift interface is not tied to the Swift compiler that compiled the Swift code for that Swift module, as ABI stability is respected by the C++ code in the header. In all other cases the generated C++ code in the header is assumed to be tied to the Swift compiler that compiled the Swift module, and thus the header should be regenerated when the compiler changes.

 The next few sections provide a high level overview of how Swift types and some other language constructs get bridged to C++. The exact details of how Swift language constructs are bridged will be covered by Swift evolution proposals, and additional documentation, such as this preliminary [user guide document](https://github.com/apple/swift/blob/main/docs/CppInteroperability/UserGuide-CallingSwiftFromC%2B%2B.md).

### Bridging Swift types

The generated header contains C++ class types that represent Swift’s struct, enum and class types that are exposed in the Swift module. These types provide access to methods, properties and other members using idiomatic C++ constructs, or non-idiomatic C++ constructs that allow C++ to access more functionality in a consistent manner. These types follow Swift semantics. For instance, a C++ value of Swift’s struct type gets copied and destroyed using Swift’s copy and destroy semantics. C++ also provides mechanisms that allow the programmer to move such values in a destructive way, i.e. "take" them. This ensures that C++ can interoperate with a wider range of Swift APIs and types, such as Swift’s move-only types.

Protocol types also get exposed to C++. They provide access to their protocol interface to C++. The generated header also provides facilities to combine protocol types into a protocol composition type. The protocol composition type provides access to the combined protocol interface in C++. 

### Bridging generics

Swift generic functions and types get bridged to C++ as C++ function and class templates. A generated C++ template instantiates a type-checked generic interface that uses the underlying polymorphic semantics that generics require when Swift APIs are called from the generated header. Type-checking is performed using the `requires` clause introduced in C++20. When C++17 and earlier is used, type-checking is performed using other legacy methods, like `enable_if` and `static_assert`. The two type-checking methods are compatible with the delayed template parsing compiler feature that Clang uses when building for Windows.

To help achieve the performance goals outlined in the prior section, the generated class templates specialize the storage for the underlying Swift generic type when the Swift API that is exposed to C++ contains such a bounded Swift generic type. This ensures that non-resilient bounded generic values can be stored inline in a C++ type that represents the underlying Swift type, instead of being boxed on the heap.  

The Swift compiler generates specializations for a specific template if the Swift module pre-specializes the specific generic declaration. These C++ specializations invoke pre-specialized Swift generic code.

### Standard library support

The Swift standard library contains a lot of useful functions and types that get bridged to C++. The generated standard library bindings enhance various Swift types like `Optional` and `Array` to provide idiomatic C++ APIs that allow the user to use such types in an idiomatic manner from C++.

### Using Swift types in C++ templates

The generated header is useful in mixed-language projects as C++ sources can include it, allowing C++ to call Swift APIs. However the C++ interoperability project also provides support for calling C++ APIs from Swift. In certain cases such C++ APIs contain function or class templates that need to be instantiated in Swift. Swift gives the user the ability to use Swift types in such cases, so the C++ templates have to be instantiated with Swift types. This means that the Swift types need to be translated into their C++ counterparts, which could then be used inside the instantiated C++ template specialization. This Swift-to-C++ type translation is performed using the same mechanism that’s used by the Swift compiler to generate the header with C++ interface for a Swift module. This means that a C++ template instantiated with a Swift type will see the same C++ representation of that Swift type regardless of whether it was instantiated from C++, or from Swift.

## Evolution process

The approach and the goals for how Swift APIs get bridged to C++ are outlined above. Each distinct Swift language construct that’s bridged to C++ will need to be covered by a detailed evolution proposal. These evolution proposals can refer to this vision document as a general context document for how Swift APIs should be bridged to C++. Every Swift API pattern that has a distinct mapping in C++ will also need a detailed and self-contained evolution proposal as well. The design for how each district language feature or API pattern is bridged to C++ is ratified only once its respective proposal goes through the Swift evolution process and is accepted by the Swift community.

## The Swift ecosystem

As a supported language feature, C++ and Swift interoperability must work well on every platform supported by Swift. In similar vein, tools in the Swift ecosystem should be updated to support C++ interoperability. More specifically, tools in this ecosystem should know how to deal with C++ code that imports the compatibility header that’s generated by the Swift compiler. Tools should also understand that C++ types and functions in the generated header act as wrappers around underlying Swift types and functions.

### Build tool support

The Swift package manager (SwiftPM) is one of the most commonly used ways to build Swift code. Swift package manager can also build C and C++ code. SwiftPM should provide good support for bridging Swift APIs to C++ out of the box. It should generate a compatibility header for a Swift package when it determines that the C++ code in the same or dependent package includes such a header, and it should ensure that this header can be found when the C++ code is compiled.

CMake is a widely used tool used for configuring and building C++ code. CMake should provide good support for adding Swift code to C++ CMake targets. Swift’s ecosystem as a whole should ensure that it should be as straightforward as possible to add support for bridging Swift APIs to C++ within the same CMake project. The C++ interoperability workgroup should provide an example CMake project that shows how Swift and C++ can interoperate between each other.

### Debugging support

Debugging support is critical for great user experience. LLDB should understand that C++ types in the generated header are just wrappers around Swift values. It should be able to display the underlying Swift value in the debugger when the user tries to inspect the C++ value that stores the Swift value in the debugger. In addition to that, the generated compatibility header should be correctly annotated to ensure that C++ inline thunks that call Swift APIs can be skipped when a user steps in or steps out into or from a call when debugging a program.

### IDE support

SourceKit-LSP is a language server that provides cross-platform IDE support for Swift code in the Swift ecosystem. It can also act as a language server for C, Objective-C and C++ as it can wrap around and redirect queries to clangd. This in turn allows SourceKit-LSP to provide support for mixed-language IDE queries, like jump-to-definition, that allows the IDE client to jump from an Objective-C method to call to its underlying Swift implementation. SourceKit-LSP should provide an equivalent level of support for C++ interoperability as well. It should understand that certain C++ declarations that get indexed by Clang are just wrappers around the declarations in Swift. This should ensure that IDE features like jump-to-definition can operate across the C++ and Swift language boundary in an IDE client that uses SourceKit-LSP.
