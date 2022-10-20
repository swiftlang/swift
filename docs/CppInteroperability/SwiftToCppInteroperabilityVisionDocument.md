# Bridging Swift to C++

## Introduction

This vision document presents a high level overview of the “reverse” (i.e. using Swift APIs from C++) half of the C++ interoperability Swift language feature. It highlights the key principles and goals that determine how Swift APIs are exposed to C++ users. It also outlines the evolution process for this feature. This document does not present the final design for how Swift APIs get mapped to C++ language constructs. The final design of this feature will evolve as this feature goes through the Swift evolution process.

This document does not cover the “forward” (i.e. using C++ APIs from Swift) half of the C++ interoperability feature, as it’s already covered by a sibling document that can be found here: https://github.com/apple/swift/pull/60501 (https://github.com/apple/swift/pull/60501/files).

This document is a prospective feature vision document, as described in the [draft review management guidelines](https://github.com/rjmccall/swift-evolution/blob/057b2383102f34c3d0f5b257f82bba0f5b94683d/review_management.md#future-directions-and-roadmaps) of the Swift evolution process. It has not yet been approved by the Language Workgroup.

## Overview of How Swift Is Bridged to C++

Swift and C++ have different approaches to interoperability with each other. The Swift compiler embeds a copy of the Clang compiler, which is then used to import C and Objective-C declarations into Swift for Objective-C interoperability. Similarly to Objective-C, Swift uses the embedded Clang compiler to import C++ declarations when the user wants to access C++ APIs from Swift. Swift also allows Objective-C code to call into Swift declarations annotated with the @objc attribute. Instead of being embedded into Clang, Swift generates an Objective-C compatibility header that contains the Objective-C declarations that the Swift programmer intended to expose to Objective-C. 

The existing model of generating a header file that can then be imported by Objective-C code can be adopted to bridge Swift declarations to C++ as well. The Clang compiler knows how to emit calls to functions with the Swift calling convention, so it’s possible to call into Swift from C++ using the matching ABI that the Swift program expects. This in turn allows the Swift compiler to generate a C++ header that is able to call into Swift functions, methods, initializers and accessors correctly. These raw calls that use the Swift calling convention can then be wrapped in inline thunk stub functions, allowing a C++ programmer to call into Swift APIs using the familiar C++ function and member function call syntax. Swift types can also be mapped to C++ types, allowing a C++ programmer to use and manipulate Swift values using familiar syntax from C++. The exact details of how Swift types and other language constructs get mapped to C++ are not covered in this document, as there’s another document that describes this: https://github.com/apple/swift/blob/main/docs/CppInteroperability/UserGuide-CallingSwiftFromC%2B%2B.md .

## Goals

Swift and C++ are both feature rich programming languages. While they both have a lot of similar features, Swift has some features which C++ can not represent natively. For instance, Swift supports overloading based on argument labels, but C++ does not. A gap like this can be bridged in an idiomatic manner between the two languages, as the C++ name of the exposed Swift function can be modified to reflect the Swift argument labels. However, some Swift features like result builders can not be bridged in an idiomatic manner to C++. Therefore, we can assume that it is impractical for Swift to expose all Swift APIs to C++. Additionally, some Swift API patterns, like conforming Swift types to specific protocols, require advanced compiler features that are impractical to support in a generated header. Thus, we can assume that it is also impractical for Swift to bridge all Swift API patterns into C++.

For the reasons outlined above, it is not a goal to expose every Swift API and language construct to C++, and it is also not a goal to make every Swift library accessible to C++. There will certainly be libraries that will lend themselves well to an ergonomic and an idiomatic C++ interface, however, there will always be some libraries that rely on language features or API patterns that can’t be bridged to C++ in a reasonable manner. 

It is a goal to allow mixed Swift and C++ projects to call their own Swift APIs that lend themselves well to interoperability from C++, and do it in a safe, performant and ergonomic manner. It is also a goal to allow these exposed Swift APIs to use Swift types from other Swift libraries (including the Swift standard library), even if the other Swift library wasn’t developed with C++ interoperability in mind. 

When Swift APIs get exposed to C++, the Swift compiler and the language specification should follow several key principles that are presented below.

### Safety

Safety is a top priority for the Swift programming language. Swift code expects its caller to adhere to Swift’s type rules and Swift’s memory model, regardless of whether it’s called from Swift or C++. Thus, the C++ code that calls Swift should properly enforce Swift’s expected language contracts. Such enforcement is needed to minimize the risk of Swift behaving in an unexpected manner at run-time when it’s being called from C++, as such behavior could lead to buggy program behavior or crashes that a Swift programmer would not expect to see in their Swift code. This kind of enforcement does not prevent all possible issues though. For instance, a bug in user’s C++ code could accidentally overwrite a Swift object stored on the heap, which could cause unexpected behavior or crashes in Swift code. To catch bugs like this one, the user should use other program analysis tools such as address sanitizer.

Swift expects that values of correct types are passed to Swift APIs. For instance, for calls to generic functions, Swift expects the caller to pass a value of type that conforms to all of the required generic requirements. Type safety should be enforced from C++ as well. The C++ compiler should verify that correct types are passed to the Swift APIs as they’re invoked from C++. A program that tries to pass an incorrect Swift type into a Swift function should not compile. If type verification can not be done at compile-time for such program, the program should trap when an invalid call is performed.

Memory safety is paramount for Swift code. Swift automatically manages the lifetime of value and reference types in Swift. These principles should translate to C++ as well. The lifetime of Swift values that are created or passed around in C++ should be managed automatically. For instance, the generated C++ code should increment the retain count of a Swift class instance when a Swift class type value is copied in C++, and it should decrement the retain count of a Swift class instance when a Swift class type value is destroyed in C++. The default convention for using Swift types in C++ should discourage dangling references and other kinds of memory bugs as much as possible.

Swift’s memory safety model also requires exclusive access to a value in order modify that value. For instance, the same value can not be passed to two inout parameters in the same function call. Swift enforces exclusivity using both compile-time and run-time checks. The generated run-time checks trap when an exclusivity violation is detected at runtime. Calls into Swift APIs from C++ should verify exclusivity for Swift values as well.

### Performance

Swift to C++ bridging should be as efficient as possible. The Swift compiler should avoid unnecessary overhead for calling into Swift functions from C++, or using Swift types from C++. Furthermore, the bridging code should not convert Swift types into their C++ counterparts automatically as that can add a lot of overhead. For instance, a Swift function returning a String should still return a Swift String in C++, and not a std::string.

Certain Swift features do not have a corresponding C++ mapping which can impose some additional overhead to make them work in C++. Resilient value types are a good example of this, as their layout and size is not known at compile time. This means that the generated C++ type that represents such a Swift type might need to impose additional performance overhead by allocating the storage for such value dynamically on the heap (i.e. boxing). In cases like this, the Swift compiler should strive to have minimal possible overhead, by finding solutions where this overhead can be optimized out for common cases.

### Achieving Safety With Performance In Mind

Certain aspects of Swift’s memory model impose certain restrictions that create tension between the goal of achieving safety and the goal of avoiding unnecessary overhead for calling into Swift from C++. Checking for exclusivity violations is a good example of this, as the C++ compiler does not have a notion of exclusivity it can verify, meaning that it is difficult to prove that a value is accessed exclusively in the C++ code that calls into Swift. This means that the C++ code that calls into Swift APIs will most likely require more run-time checks to validate exclusivity than similar Swift code that calls the same Swift APIs.

The adherence to Swift’s type and memory safety rules should be prioritized ahead of performance when C++ calls into Swift, even if this means more run-time checks are required. This is needed to avoid unexpected crashes or other unexpected behavior in users Swift code. For users seeking maximum performance, Swift provides additional compiler flags that avoid certain run-time checks. Those flags should be taken into account when the C++ header is generated. An example of such flag is -enforce-exclusivity. When  -enforce-exclusivity=none is passed to Swift, the Swift compiler does not emit any run-time checks that check for exclusivity violations. A flag like this should also affect the generated C++ header, and the Swift compiler should not emit any run-time checks for exclusivity in the generated C++ header when this flag is used.

### Ergonomics

Swift APIs should be mapped over to C++ language features that have a direct correspondence to the Swift language feature. In cases where there isn’t a directly corresponding C++ language feature, the Swift compiler should provide a reasonable approximation to the original Swift language feature using other C++ constructs. For example, Swift’s enum type can contain methods and nested types, and such constructs can’t be represented by a single C++ enum type in an idiomatic manner. Swift’s enum type can be mapped to a C++ class instead that allows both enum-like switch statement behavior and also enables the C++ user to invoke member functions on the Swift enum value and access its nested types from C++.

The C++ representation of certain Swift types should be appropriately enhanced to allow them to be used in an idiomatic manner. For instance, it should be possible to use Swift’s Array type (or any type that conforms to Collection) in a ranged for loop in C++. Such enhancements should be done with safety in mind, to ensure that Swift’s memory model is not violated.

There should be no differences on the C++ side between using libraries that opt-in into library evolution and libraries that don’t, except in specific required cases, like checking the unknown default case of a resilient enum.

### Clear Language Mapping Rules

C++ is a very expressive language and it can provide representation for a lot of Swift language constructs. Not every Swift language construct will map to its direct C++ counterpart. For instance, Swift initializers might get bridged to static init member functions instead of constructors in C++, to allow C++ code to call failable initializers in a way that’s consistent with other initializer calls. Therefore, it’s important to provide documentation that describes how Swift language constructs get mapped to C++. It is a goal of C++ interoperability to provide a clear and well-defined mapping for how Swift language constructs are mapped to C++. Additionally, it is also a goal to clearly document which language constructs are not bridged to C++. In addition to documentation, compiler diagnostics should inform the user about types or functions that can’t be exposed to C++, when the user wants to expose them explicitly. It is a goal of C++ interoperability to add a set of clear diagnostics that let the user know when a certain Swift declaration is not exposed. It is not a goal to diagnose such cases when the user did not instruct the compiler to expose a declaration explicitly. For example, the Swift compiler might not diagnose when an exposed Swift type does not expose one of its public methods to C++ due to its return type not being exposed if such method does not have an explicit annotation that instructs the compiler that this method must be exposed to C++.

Some Swift APIs patterns will map to distinct C++ language constructs or patterns. For instance, an empty Swift enum with static members is commonly used in a namespace-like manner in Swift. This kind of enum can be mapped to a C++ namespace. It is a goal of C++ interoperability to provide a clear mapping for how Swift API patterns like this one are bridged to C++.

The semantics of how Swift types behave should be preserved when they’re mapped to C++. For instance, in C++, there should still be a semantic difference between Swift value and reference types. Additionally, Swift’s copy-on-write data types like Array should still obey the copy-on-write semantics in C++.

### Swift API Design Should be Unaffected

Swift API authors should not change the way they write Swift code and design Swift APIs based on how specific Swift language constructs are exposed to C++. They should use the most effective Swift constructs for the task. C++ interop should provide best effort view for these constructs without imposing decisions or restrictions that might limit the Swift API designers and authors.

### Objective-C Support

The existing Swift to Objective-C bridging layer should still be supported even when C++ bindings are generated in the generated header. Furthermore, the generated C++ bindings should use appropriate Objective-C++ types or constructs in generated C++ declarations where it makes sense to do so. For instance, a Swift function that returns an Objective-C class instance should be usable from an Objective-C++ compilation unit.

## The Approach

The Swift compiler exposes Swift APIs to C++ by generating a header file that contains C++ declarations that wrap around the underlying calls into Swift code and provide a suitable representation for Swift types. Typically, a single header file is generated for one Swift module.

The generated header file depends on specific LLVM and Clang compiler features for Swift calling convention support, and thus it can only be compiled by Clang. The header does not depend on any Clang-specific C++ language extensions. The header might depend on other Clang-only features that improve developer experience for the C++ users, like specific attributes that improve diagnostics.

The generated header file uses advanced C++ features that require a recent C++ language standard. C++20 is the recommend language standard to use for Swift APIs, however, the generated header should also be compatible with C++17 and C++14.

The generated header file also contains the C and Objective-C declarations that Swift exposes to C and Objective-C on platforms that support C or Objective-C interoperability. Thus a single header file can be used by codebases that mix C, Objective-C and C++.

The generated header file is a temporary build artifact. On platforms with ABI stability, the generated C++ code for an ABI stable Swift interface is not tied to the Swift compiler that compiled the Swift code for that Swift module, as ABI stability is respected by the C++ code in the header. In all other cases the generated C++ code in the header is assumed to be tied to the Swift compiler that compiled the Swift module, and thus the header should be regenerated when the compiler changes.

### Bridging Swift Types

The generated header contains C++ class types that represent Swift’s struct , enum and class types that are exposed in the Swift module. These types provide access to methods, properties and other members using idiomatic C++ constructs, or non-idiomatic C++ constructs that allow C++ to access more functionality in a consistent manner. These types follow Swift semantics. For instance, a C++ value of Swift’s struct type gets copied and destroyed using Swift’s copy and destroy semantics. C++ also provides mechanisms that allow the programmer to move such values in a destructive way, i.e. “take” them. This ensures that C++ can interoperate with a wider range of Swift APIs and types, such as Swift’s move only types.

Protocol types also get exposed to C++. They provide access to their protocol interface to C++. The generated header also provides facilities to combine protocol types into a protocol composition type. The protocol composition type provides access to the combined protocol interface in C++. 

### Bridging Generics

Swift generic functions and types get bridged to C++ as C++ function and class templates. A generated C++ template instantiates a type-checked generic interface that uses the underlying polymorphic semantics that generics require when Swift APIs are called from the generated header. Type-checking is performed using the requires clause introduced in C++20. When C++17 and earlier is used, type-checking is performed using other legacy methods, like enable_if and static_assert.

To help achieve the performance goals outlined in the prior section, the generated class templates specialize the storage for the underlying Swift generic type when the Swift API that is exposed to C++ contains such a bounded Swift generic type. This ensures that non-resilient bounded generic values can be stored inline in a C++ type that represents the underlying Swift type, instead of being boxed on the heap.  

The Swift compiler generates specializations for a specific template if the Swift module pre-specializes the specific generic declaration. These C++ specializations invoke pre-specialized Swift generic code.

### Standard Library Support

The Swift standard library contains a lot of useful functions and types that get bridged to C++. The generated standard library bindings enhance various Swift types like Optional and Array to provide idiomatic C++ APIs that allow the user to use such types in an idiomatic manner from C++. 

### Using Swift Types in C++ Templates

The generated header is useful in mixed language projects as C++ sources can include it, allowing C++ to call Swift APIs. However the C++ interoperability project also provides support for calling C++ APIs from Swift. In certain cases such C++ APIs contain function or class templates that need to be instantiated in Swift. Swift gives the user the ability to use Swift types in such cases, so the C++ templates have to be instantiated with Swift types. This means that the Swift types needs to be translated into their C++ counterparts, which can then be used inside the instantiated C++ template specialization. This Swift to C++ type translation is performed using the same mechanism that’s used by the Swift compiler to generate the C++ compatibility header. This means that a C++ template instantiated with a Swift type will see the same C++ representation of that Swift type regardless of whether it was instantiated from C++, or from Swift.

## Evolution Process

The approach and the goals for how Swift APIs get bridged to C++ are outlined above. Each distinct Swift language construct that’s bridged to C++ will need to be covered by a detailed evolution proposal. These evolution proposals can refer to this vision document as a general context document for how Swift APIs should be bridged to C++. Every Swift API pattern that has a distinct mapping in C++ will also need a detailed and self-contained evolution proposal as well. The design for how each district language feature or API pattern is bridged to C++ is ratified only once its respective proposal goes through the Swift evolution process and is accepted by the Swift community.

## The Swift Ecosystem

As a supported language feature, C++ and Swift interoperability must work well on every platform supported by Swift. In similar vein, tools in the Swift ecosystem should be updated to support C++ interoperability. More specifically, tools in this ecosystem should know how to deal with C++ code that imports the compatibility header that’s generated by the Swift compiler. Tools should also understand that C++ types and functions in the generated header act as wrappers around underlying Swift types and functions.

### Build Tool Support

The Swift package manager (SwiftPM) is one of the most commonly used ways to build Swift code. Swift package manager can also build C and C++ code. SwiftPM should provide good support for bridging Swift APIs to C++ out of the box. It should generate a compatibility header for a Swift package when it determines that the C++ code in the same or dependent package includes such a header, and it should ensure that this header can be found when the C++ code is compiled.

CMake is a widely used tool used for configuring and building C++ code. CMake should provide good support for adding Swift code to C++ CMake targets. Swift’s ecosystem as a whole should ensure that it should be as straightforward as possible to add support for bridging Swift APIs to C++ within the same CMake project. The C++ interoperability workgroup should provide an example CMake project that shows how Swift and C++ can interoperate between each other.

### Debugging Support

Debugging support is critical for great user experience. LLDB should understand that C++ types in the generated header are just wrappers around Swift values. It should be able to display the underlying Swift value in the debugger when the user tries to inspect the C++ value that stores the Swift value in the debugger. In addition to that, the generated compatibility header should be correctly annotated to ensure that C++ inline thunks that call Swift APIs can be skipped when a user steps in or steps out into or from a call when debugging a program.

### IDE Support

Sourcekit-lsp is a language server that provides cross-platform IDE support for Swift code in the Swift ecosystem. It can also act as a language server for C, Objective-C and C++ as it can wrap around and redirect queries to Clangd. This in turn allows Sourcekit-lsp to provide support for mixed-language IDE queries, like jump-to-definition, that allows the IDE client to jump from an Objective-C method to call to its underlying Swift implementation. Sourcekit-lsp should provide an equivalent level of support for C++ interoperability as well. It should understand that certain C++ declarations that get indexed by Clang are just wrappers around the declarations in Swift. This should ensure that IDE features like jump-to-definition can operate across the C++ and Swift language boundary in an IDE client that uses sourcekit-lsp.
