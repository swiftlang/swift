# Deprecation Warnings (`deprecated`)

The deprecated group is a supergroup designed to manage all kinds of warnings related to the use of deprecated elements. This group can include other diagnostic groups with similar meanings. The deprecated group includes the following groups:

- `availability_deprecated`: Includes warnings for APIs marked as deprecated.

## Usage Example

```sh
swiftc -Werror deprecated file.swift
swiftc -warnings-as-errors -Wwarning deprecated file.swift
```