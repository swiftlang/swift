# Refining with API Notes

API Notes provide a powerful way to refine how Swift imports a C API without modifying the original header files. This external metadata approach is essential when you work with C libraries where you cannot change the source code.

## Overview

API Notes are text files written in YAML that provide the Swift compiler with the same semantic information as inline C attributes. You place an API Notes file alongside the `module.modulemap` in your module's directory, and the Swift compiler automatically discovers and applies it during import.

The file must be named after the module it describes, with a `.apinotes` extension. For our case study, the file would be named `WebGPU.apinotes`.

This method is the perfect solution for system libraries, pre-compiled binaries, or any C library whose source you prefer not to alter.

### How It Works

Inside the API Notes file, you identify C declarations by name and apply attributes to them using YAML syntax. The structure of the file mirrors the structure of the C code, with top-level keys for `Functions`, `Globals`, `Tags` (structs, enums, and unions), and more.

Let's revisit the `wgpuDeviceGetQueue` function and apply the same `swift_name` transformation using API Notes instead of C attributes.

Create a file named `WebGPU.apinotes` and add the following content:

```yaml
# WebGPU.apinotes

Name: WebGPU
Functions:
  - Name: wgpuDeviceGetQueue
    SwiftName: "getter:WGPUDevice.queue(self:)"
```

This YAML structure tells the Swift compiler:
1.  **`Name: WebGPU`**: This file contains notes for the `WebGPU` module.
2.  **`Functions:`**: The following notes apply to global functions.
3.  **`- Name: wgpuDeviceGetQueue`**: This specific note is for the function named `wgpuDeviceGetQueue`.
4.  **`SwiftName: "..."`**: This is the API Notes equivalent of the `swift_name` attribute. It applies the exact same transformation rules as the C attribute, resulting in an identical Swift interface.

### When to Use API Notes

API Notes give you the full power of C attributes with the flexibility of keeping the original headers untouched.

**Advantages:**
- **Non-Invasive:** You can refine any C library without altering its source code, which is ideal for third-party or system frameworks.
- **Clean Separation:** It keeps the C headers clean of compiler-specific syntax, which can be beneficial for multi-platform C projects.

**Considerations:**
- **Indirect Connection:** The metadata is physically separate from the C declaration, which can sometimes make it harder to see the connection between the note and the code it affects.

You now have two powerful mechanisms for refining a C API. The following sections will provide practical, side-by-side examples of both C attributes and API Notes for every transformation you learn.