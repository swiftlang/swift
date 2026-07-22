# Enabling C++ interoperability in your project

Bring a C++ class into Swift under a Swift-friendly name.

## Overview

When you enable C++ interoperability, your C++ class names, methods, and conventions translate to Swift exactly as the C++ author wrote them. That specificity can clash with Swift's naming expectations or with types that already exist in your project. The bridging macros in `swift/bridging` let you reshape that boundary so the C++ API reads like idiomatic Swift.

This article shows how to enable interoperability, expose a C++ class to Swift through a module, and use the `SWIFT_NAME` macro to give that class a Swift-friendly name. You write the same Swift code to use the class whether you choose Swift Package Manager, the command line, or Xcode, and the workflow is the same on macOS, Linux, and Windows.

The running example is a C++ class called `Error`. Because Swift already has its own `Error`, you rename it to `CxxErrorExample` as it crosses into Swift, and expose it through a module named `ErrorLib`.

## Enable C++ interoperability

Interoperability is off by default. Turning it on tells the Swift compiler to read your C++ declarations and make them callable from Swift.

If you're using Swift Package Manager, add `.interoperabilityMode(.Cxx)` to the `swiftSettings` of the Swift target that imports your C++ code, not the C++ library target itself. You write the full `Package.swift` when you expose the module.

For command-line users, pass `-cxx-interoperability-mode=default` to `swiftc` when you compile the project.

If you're using Xcode, set the C++ and Objective-C Interoperability build setting to C++ / Objective-C++ on both your target and your project.

## Create your C++ class

If you're using Swift Package Manager, put `Error.hpp` in your C++ target's public-headers directory, `Sources/ErrorLib/include/`, and `Error.cpp` in `Sources/ErrorLib/`.

If you're building from the command line, create `Error.hpp` and `Error.cpp` in your project directory.

If you're using Xcode, add a C++ file with File > New > File from Template… and check Also create a header file if the template doesn't make one. Add the file to your app target. If Xcode offers to create a bridging header, decline it. You don't need one.

A bridging header and a module map do the same job of exposing your C++ to Swift, so you only need one. This guide uses the module map you create next: it's the standard mechanism on macOS, Linux, and Windows, and it works in Xcode just as well.

Define your class in a header, and add `#include <swift/bridging>` at the top. That header ships with the Swift toolchain and the compiler finds it automatically, with no extra search path to set. It's what gives you the bridging macros, including `SWIFT_NAME`, which controls how the class crosses over into Swift.

Place `SWIFT_NAME` at the end of the declaration you want to rename: for a class, after the closing brace and before the semicolon; for a function, at the end of the declaration. Here you rename the class to `CxxErrorExample` so it doesn't collide with Swift's own `Error`. The macro only changes the name Swift sees. The C++ name stays `Error` for your C++ code.

The completed `Error.hpp` looks like this:

```cpp
#ifndef Error_hpp
#define Error_hpp

#include <stdio.h>
#include <string>
#include <swift/bridging>

class Error {
public:
    std::string print() const;
} SWIFT_NAME(CxxErrorExample);

#endif /* Error_hpp */
```

The matching implementation in `Error.cpp` defines the method:

```cpp
#include "Error.hpp"

std::string Error::print() const {
    return "Error message!";
}
```

Notice that `print()` is declared `const`. That matters on the Swift side, as the next section shows when you use the class.

## Expose the class through a module

Swift imports C++ as a module: a module is the unit Swift brings in with `import`, and a module map is how you present a set of C++ headers to Swift as one importable unit.

The module name can't clash with an existing Swift import, type, or the name of your class. The class here is `Error`, so you name the module `ErrorLib` to keep it distinct from Swift's `Error`.

@TabNavigator {
    @Tab("Swift Package Manager") {
        You don't write `module.modulemap` by hand. Swift Package Manager generates the module from your C++ target's public headers in `Sources/ErrorLib/include/`. Lay the package out like this:

        ```
        ErrorLib/
        ├── Package.swift
        └── Sources/
            ├── ErrorLib/
            │   ├── include/Error.hpp
            │   └── Error.cpp
            └── ErrorExample/
                └── main.swift
        ```

        Declare a C++ library target for the header and source. As well as a Swift executable target that depends on it and turns on interoperability:

        ```swift
        // swift-tools-version: 6.0

        import PackageDescription

        let package = Package(
            name: "ErrorLib",
            products: [
                .library(
                    name: "ErrorLib",
                    targets: ["ErrorLib"]
                ),
            ],
            targets: [
                .target(
                    name: "ErrorLib",
                    path: "Sources/ErrorLib",
                    publicHeadersPath: "include",
                    cxxSettings: [
                        .headerSearchPath("include")
                    ]
                ),
                .executableTarget(
                    name: "ErrorExample",
                    dependencies: ["ErrorLib"],
                    swiftSettings: [
                        .interoperabilityMode(.Cxx)
                    ]
                ),
            ],
            cxxLanguageStandard: .cxx17
        )
        ```

        Because `ErrorExample` depends on `ErrorLib` and turns on interop, Swift finds the `ErrorLib` module through the package graph and imports your C++ class. There's no `-I` flag or Import Paths setting to add.
    }
    @Tab("Command line") {
        Save `module.modulemap` in the same directory as your header file. Next to `header`, give the name of your header file; since this is C++, it ends in `.hpp`. You point Swift at its directory with `-I` when you compile.

        ```
        module ErrorLib {
            header "Error.hpp"
            export *
        }
        ```
    }
    @Tab("Xcode") {
        Add a new empty file named `module.modulemap` in the same directory as your header file with File > New > File from Template… and add it to your target. Next to `header`, give the name of your header file; since this is C++, it ends in `.hpp`.

        ```
        module ErrorLib {
            header "Error.hpp"
            export *
        }
        ```

        In the target's build settings, add the directory that contains `module.modulemap` to Import Paths (the `SWIFT_INCLUDE_PATHS` build setting), for example `$(SRCROOT)/YourFolderName`. This is the Xcode equivalent of the command-line `-I` flag. Without it, Swift can't locate the module map that defines `ErrorLib`, and the build fails.
    }
}

## Use the C++ module from Swift

In a Swift file, import the module by name:

```swift
import ErrorLib
```

`ErrorLib` is the module name you set up in the previous step. That's what gives Swift something to `import`.

Now create an instance and call its method in `main.swift`:

```swift
import ErrorLib

let error = CxxErrorExample()
let message = String(error.print())

print(message)
```

Here you can use a `let` instead of a `var`. Swift imports a `const` C++ member function as a non-mutating method, so calling `error.print()` doesn't require the instance to be mutable. If you drop `const` from `print()`, Swift imports it as `mutating`. A mutating call on a `let` fails to compile, so you need a `var`. The `const` you wrote earlier is what lets this read like ordinary, idiomatic Swift.

The return value comes across just as smoothly, because `std::string` bridges to Swift's `String` for free. The `print()` method returns a C++ `std::string`, and Swift's C++ standard-library interoperability gives you a `String` initializer that converts it directly. That's all `String(error.print())` is doing, and you don't need to import anything extra for it.

Calling the method also looks like any other Swift type: `error.print()` is just a method call, exactly like calling a method on a native Swift value, and the interop boundary disappears at the call site.

If you're using Swift Package Manager, run the executable with `swift run ErrorExample` from the package's root directory, the one containing `Package.swift`. Here, `ErrorExample` is the name of the executable target rather than the package, which is `ErrorLib`. Swift resolves the `import` through the package graph, and `CxxErrorExample` is available throughout the target. This prints `Error message!`.

For command-line users, compil e the Swift and C++ together and run the result. The `-I .` points Swift at the directory holding `module.modulemap`, and a single invocation compiles both languages. On macOS, prefix the command with `xcrun` so it uses your installed Xcode toolchain; on Linux and Windows, call `swiftc` directly:

```
# macOS
xcrun swiftc -cxx-interoperability-mode=default -I . main.swift Error.cpp -o main

# Linux and Windows
swiftc -cxx-interoperability-mode=default -I . main.swift Error.cpp -o main

./main
```

This prints `Error message!`.

If you're using Xcode, build and run. The `import` resolves through your module map, and `CxxErrorExample` is available throughout the target. The app prints `Error message!`.

## Manage the instance's lifetime

Swift imports a C++ class or struct as a value type by default, so `error` behaves like a Swift struct: there's no ARC and no manual `delete`, and Swift calls the C++ destructor when `error` goes out of scope. If you instead want reference semantics, `swift/bridging` provides macros like `SWIFT_SHARED_REFERENCE.`
