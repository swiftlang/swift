# SPI import ignored (SPIImportIgnored)

## Overview

A module was imported with `@_spi` but the import will not expose any SPI symbols because the module was loaded from its public interface rather than from a binary module or private interface.

SPI symbols are only available when a module is loaded from a `.swiftmodule` binary or a `.private.swiftinterface` file. When only the public `.swiftinterface` is available, the `@_spi` attribute on the import has no effect.

To resolve this warning, ensure that either the binary `.swiftmodule` or the `.private.swiftinterface` for the imported module is available on the search path, or remove the `@_spi` attribute from the import if SPI access is not needed.
