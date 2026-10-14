# Section not indexable (SectionNotIndexable)

## Overview

A `@used` variable is placed with `@section` into a section whose name the image's section index cannot describe, so `customSectionContents(named:)` will not find the section at run time.

The compiler lists every custom section that holds Swift variables in a per-image index that the runtime reads. On ELF and Wasm the index relies on the linker-defined `__start_<name>` and `__stop_<name>` symbols, which exist only for section names that are valid C identifiers (letters, digits and underscores, not starting with a digit). On Windows (COFF) the index brackets a section's records with markers in the `$A` and `$Z` groups, so the records need a `$` group that sorts between them, such as `.myplugins$B`.

The variable is still placed in the named section and the linker still handles it; only run-time lookup by name is affected. Code that reads the section through its own linker symbols or a linker script is unaffected.

To resolve this warning, rename the section so that the index can describe it:

```swift
// ELF and Wasm
@section("myplugins") @used let record: Record = ...

// Windows
@section(".myplugins$B") @used let record: Record = ...
```

If the section is meant for a linker script or another reader and never needs run-time lookup, drop `@used` from the variable or keep the warning; it is grouped as `SectionNotIndexable`, so `-Wwarning SectionNotIndexable` keeps it a warning under `-warnings-as-errors`. Embedded Swift emits no section index and does not issue this warning.
