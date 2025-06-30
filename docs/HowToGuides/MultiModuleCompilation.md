# Multi-Module Compilation Guide

This guide describes how to quickly compile minimal multi-module Swift projects without setting up Xcode projects or Swift packages. This is particularly useful when creating minimal reproducers for compiler issues or when you need to test module interactions directly.

## Overview

When creating minimal reproducers for issues, occasionally it is helpful to "hand-compile" a multi-module project quickly (say via a small shell script) without setting up an Xcode project or a Swift package. This guide shows you how to do this using the Swift compiler directly.

## Basic Steps

### 1. Compile Dependency Module
```bash
swiftc mymodule.swift -module-name MyModule -emit-library -o /path/to/mymodule.dylib -emit-module-path /path/to/swiftmoduledir/
```

**Note**: The `-module-name` flag is required to specify the module name. The module file will be created as `mymodule.swiftmodule` and may need to be renamed to match the module name.

### 2. Compile Main Executable
```bash
swiftc mymainmodule.swift -emit-object -o /path/to/mymainmodule.o -I /path/to/swiftmoduledir/
```

### 3. Link Executable
```bash
swiftc /path/to/mymainmodule.o /path/to/mymodule.dylib -emit-executable -o /path/to/myexe
```

### 4. Run with Local Runtime (Development)
```bash
env DYLD_LIBRARY_PATH=/path/to/libswiftCore.dylib /path/to/myexe
```

## Platform Differences

- **macOS**: Use `.dylib` extensions, `DYLD_LIBRARY_PATH`
- **Linux**: Use `.so` extensions, `LD_LIBRARY_PATH`

## Additional Options

- Import module directories: `-I /path/to/module/directory`
- Import Objective-C headers: `-import-objc-header /path/to/header.h`

## Complete Example

```bash
# Create module
swiftc mymodule.swift -module-name MyModule -emit-library -o libmymodule.dylib -emit-module-path ./

# Rename module file to match module name (if needed)
mv mymodule.swiftmodule MyModule.swiftmodule

# Compile main
swiftc main.swift -emit-object -o main.o -I ./

# Link
swiftc main.o libmymodule.dylib -emit-executable -o myapp

# Run
./myapp
```

## Troubleshooting

### Module Not Found
If you get a "module not found" error, ensure that:
- The `-emit-module-path` directory contains the module files
- The `-I` path points to the correct module directory
- The module name in your import statement matches the module name

### Library Not Found
If you get a "library not found" error at runtime:
- Ensure the library path is correct
- On macOS, you may need to set `DYLD_LIBRARY_PATH`
- On Linux, you may need to set `LD_LIBRARY_PATH`

### Linking Errors
If you get linking errors:
- Ensure all required libraries are specified in the link command
- Check that the library architectures match your target
- Verify that the object file and library are compatible

## Related Documentation

- [Debugging The Compiler](DebuggingTheCompiler.md) - For more information about debugging Swift compiler issues
- [Swift Driver Documentation](Driver.md) - For detailed information about the Swift compiler driver 
