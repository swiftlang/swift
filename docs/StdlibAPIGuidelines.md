Swift Standard Library API Design Guide
=======================================

> **note**
>
> This guide documents *current practice* in the Swift
>
> :   standard library as of April 2015. API conventions are expected to
>     evolve in the near future to better harmonize with Cocoa.
>
The current Swift Standard Library API conventions start with the Cocoa
guidelines as discussed on these two wiki pages: \[[API
Guidelines](http://cocoa.apple.com/cgi-bin/wiki.pl?API_Guidelines),
[Properties](http://cocoa.apple.com/cgi-bin/wiki.pl?Properties)\], and
in this [WWDC Presentation](http://cocoa.apple.com/CocoaAPIDesign.pdf).
Below, we list where and how the standard library's API conventions
differ from those of Cocoa

Differences
-----------

Points in this section clash in one way or other with the Cocoa
guidelines.

### The First Parameter

-   The first parameter to a function, method, or initializer typically
    does not have an argument label:
-   Typically, no suffix is added to a function or method's base name in
    order to serve the same purpose as a label:
-   A preposition is added to the end of a function name if the role of
    the first parameter would otherwise be unclear:
-   Argument labels are used on first parameters to denote special
    cases:

### Subsequent Parameters

-   Argument labels are chosen to clarify the *role* of an argument,
    rather than its type:
-   Second and later parameters are always labeled except in cases where
    there's no useful distinction of roles:

        swap(&a, &b)                                                    // OK

        let topOfPicture = min(topOfSquare, topOfTriangle, topOfCircle) // OK

### Other Differences

-   We don't use namespace prefixes such as “`NS`{.sourceCode}”, relying
    instead on the language's own facilities.
-   Names of types, protocols and enum cases are
    `UpperCamelCase`{.sourceCode}. Everything else is
    `lowerCamelCase`{.sourceCode}. When an initialism appears, it is
    **uniformly upper- or lower-cased to fit the pattern**:
-   Protocol names end in `Type`{.sourceCode}, `able`{.sourceCode}, or
    `ible`{.sourceCode}. Other type names do not.

Additional Conventions
----------------------

Points in this section place additional constraints on the standard
library, but are compatible with the Cocoa guidelines.

-   We document the complexity of operations using big-O notation.
-   In API design, when deciding between a nullary function and a
    property for a specific operation, arguments based on performance
    characteristics and complexity of operations are not considered.
    Reading and writing properties can have any complexity.
-   We prefer methods and properties to free functions. Free functions
    are used when there's no obvious `self`{.sourceCode} :

        min(x, y, z)

    when the function is an unconstrained generic :

        print(x)

    and when function syntax is part of the domain notation :

        -sin(x)

-   Type conversions use initialization syntax whenever possible, with
    the source of the conversion being the first argument:

        let s0 = String(anInt)            // yes
        let s1 = String(anInt, radix: 2)  // yes
        let s1 = anInt.toString()         // no

    The exception is when the type conversion is part of a protocol:

        protocol IntConvertible {
          func toInt() -> Int // OK
        }

-   Even unlabelled parameter names should be meaningful as they'll be
    referred to in comments and visible in “generated headers”
    (cmd-click in Xcode):
-   Type parameter names of generic types describe the role of the
    parameter, e.g.

### Acceptable Short or Non-Descriptive Names

-   Type parameter names of generic functions may be single characters:
-   `lhs`{.sourceCode} and `rhs`{.sourceCode} are acceptable names for
    binary operator or symmetric binary function parameters:
-   `body`{.sourceCode} is an acceptable name for a trailing closure
    argument when the resulting construct is supposed to act like a
    language extension and is likely to have side-effects:

        func map<U>(transformation: T->U) -> [U] // not this one

        func forEach<S: SequenceType>(body: (S.Generator.Element)->())

### Prefixes and Suffixes

-   `Any`{.sourceCode} is used as a prefix to denote “type
    erasure,” e.g. `AnySequence<T>`{.sourceCode} wraps any sequence with
    element type `T`{.sourceCode}, conforms to
    `SequenceType`{.sourceCode} itself, and forwards all operations to
    the wrapped sequence. When handling the wrapper, the specific type
    of the wrapped sequence is fully hidden.
-   `Custom`{.sourceCode} is used as a prefix for special protocols that
    will always be dynamically checked for at runtime and don't make
    good generic constraints, e.g.
    `CustomStringConvertible`{.sourceCode}.
-   `InPlace`{.sourceCode} is used as a suffix to denote the mutating
    member of a pair of related methods:
-   `with`{.sourceCode} is used as a prefix to denote a function that
    executes a closure within a context, such as a guaranteed lifetime:
-   `Pointer`{.sourceCode} is used as a suffix to denote a non-class
    type that acts like a reference, c.f.
    `ManagedBufferPointer`{.sourceCode}
-   `unsafe`{.sourceCode} or `Unsafe`{.sourceCode} is *always* used as a
    prefix when a function or type allows the user to violate memory or
    type safety, except on methods of types whose names begin with
    `Unsafe`{.sourceCode}, where the type name is assumed to
    convey that.
-   `C`{.sourceCode} is used as a prefix to denote types corresponding
    to C language types, e.g. `CChar`{.sourceCode}.
