# Availability Deprecation Warnings (`availability_deprecated`)

This diagnostic group includes warnings related to deprecated APIs that may be removed in future versions and should be replaced with more current alternatives.

The `availability_deprecated` group covers the following warnings:
- Use of a function annotated with `@available(<platform>, deprecated: <version>)`
  ```swift
  @available(iOS, deprecated: 10.0)
  func oldFunction() {
    // This function is deprecated and should not be used.
  }

  oldFunction() // 'oldFunction()' is deprecated
  ```
- Use of a function annotated with `@available(<platform>, deprecated: <version>, renamed: "<new name>")`
  ```swift
  @available(iOS, deprecated: 10.0, renamed: "newFunction")
  func oldFunction() {
    // This function is deprecated and should not be used.
  }

  oldFunction() // 'oldFunction()' is deprecated: renamed to 'newFunction'
  ```

## Usage Example

```sh
swiftc -Werror availability_deprecated file.swift
swiftc -warnings-as-errors -Wwarning availability_deprecated file.swift
```