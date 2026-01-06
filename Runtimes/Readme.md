# Swift Runtime Libraries

This directory contains the pieces of the Swift runtime libraries.

## Development

While we're bringing up the new standard library build, please do not commit the
standard library source files. Use the `Resync.cmake` file to copy the files as
needed.

```sh
$ cmake -P Resync.cmake
```

> [!IMPORTANT]
> Re-run this script after updating your Swift checkout to ensure that you are
> building the latest standard library sources.

Once the migration is completed, we will be deleting this script. It
is a temporary workaround to avoid trying to keep multiple sets of files in
sync.

> [!NOTE]
> This script does not add new files to any CMakeLists.txt. Any new files
> that are copied over will need to be added manually to ensure that
> they will be built by CMake.

## Layering

```
╔═══════════════╗
║               ║
║    Testing    ║
║               ║
╠───────────────╣
│               │
│ Supplemental  │
│               │
├───────────────┤
│               │
│    Overlay    │
│               │
├───────────────┤
│               │
│     Core      │
│               │
└───────────────┘
```

### Core

The _Core_ project contains the basic datatypes and underpinnings used by the
rest of the libraries that make up the standard library. The _Core_ libraries
must be built first.
The _Core_ project provides the following libraries:
 - `swiftCore`
 - `swift_Concurrency`
 - `SwiftOnoneSupport`
 - `CommandLine`
 - `Demangling`
 - `Runtime`
 - `LLVMSupport`
 - `StdlibStubs`
 - `Threading`
 - `SwiftShim`

These libraries must work across the platforms that Swift supports.

### Overlay

The Overlay project contains a few default platform overlay libraries. A
platform overlay is responsible for exposing the system libraries into Swift in
an ergonomic fashion. On most systems, this exposes the C standard library and a
few other libraries that are normally available on that system. The overlay
libraries are allowed to depend on any of the runtime libraries provided by the
_Core_ project and libraries distributed by the platform.

The platform overlay is specific to the platform that it overlays and cannot be
used across platforms.

### Supplemental

The supplemental libraries provide the remainder of the standard distribution of
libraries provided by Swift.

The Supplemental libraries include:
 - `RegexParser`
 - `StringProcessing`
 - `RegexBuilder`
 - `Cxx Interop`
 - `Synchronization`
 - `Distributed`
 - `Observation`

The behavior of these libraries may differ slightly based on the behavior of the
underlying operating system. These libraries are allowed to depend on the
platform overlay and the libraries provided by the _Core_ project.

### Testing

The testing project provides testing support for the libraries that make up the
standard library. These libraries are optional and are not intended for
shipping. They must be built last, after the Core, the Overlay, and the
Supplemental libraries.
