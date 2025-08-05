# Multi-Module Compilation Guide

This guide describes how to quickly compile minimal multi-module Swift projects without setting up Xcode projects or Swift packages. This is primarily useful for creating minimal reproducers for compiler issues or debugging module interactions.

## Basic Steps

### 1. Compile Dependency Module
```bash
swiftc mymodule.swift -module-name MyModule -emit-library -o /path/to/mymodule.dylib -emit-module-path MyModule.swiftmodule
```

**Note**: The `-module-name` flag is required to specify the module name. The `-emit-module-path` can specify the exact output filename.

### 2. Compile Main Executable
```bash
swiftc mymainmodule.swift -emit-object -o /path/to/mymainmodule.o -I /path/to/moduledir/
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

- **macOS**: Use `.dylib` for dynamic libraries
- **Linux**: Use `.so` for shared objects
- **Windows**: Use `.dll` for dynamic link libraries

## Complete Example

Create the following files:

**mymodule.swift**:
```swift
public func greet(name: String) -> String {
    return "Hello, \(name)!"
}
```

**main.swift**:
```swift
import MyModule

print(greet(name: "World"))
```

**Compilation steps**:
```bash
# Step 1: Compile module
swiftc mymodule.swift -module-name MyModule -emit-library -o libmymodule.dylib -emit-module-path MyModule.swiftmodule

# Step 2: Compile main
swiftc main.swift -emit-object -o main.o -I ./

# Step 3: Link
swiftc main.o libmymodule.dylib -emit-executable -o myexe

# Step 4: Run (development)
env DYLD_LIBRARY_PATH=/path/to/libswiftCore.dylib ./myexe
```

## Additional Options

- Import module directories: `-I /path/to/module/directory`
- Specify module name: `-module-name ModuleName`
- Emit module interface: `-emit-module-interface`

## Use Cases

This approach is particularly useful for:
- Creating minimal reproducers for compiler issues
- Debugging module interaction problems
- Testing compiler features without full project setup
- Educational purposes to understand Swift compilation

For production development, consider using Swift Package Manager or Xcode projects instead.

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
