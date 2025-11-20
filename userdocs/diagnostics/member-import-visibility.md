# Member import visibility (MemberImportVisibility)

Enables errors for uses of members that cannot be accessed because their defining module is not directly
imported.


## Overview

Prior to enabling this feature, it was possible to use a member if it was declared in a module that
was transitively imported. `MemberImportVisibility` unifies the visibility rules for members to
match those of top-level, such that only members contained in a direct import are visible:

```swift
func getFileContents() throws -> String {
  // error: initializer 'init(contentsOfFile:)' is not available due to missing import of defining module 'Foundation'
  return try String(contentsOfFile: "example.txt")
}
```

To resolve the error, add an import of the module that defines the member.


## Migration

```sh
-enable-upcoming-feature MemberImportVisibility:migrate
```

Enabling migration for `MemberImportVisibility` adds fix-its for the missing modules, i.e. those that
declare members used in the file and are not directly imported.


## See also

- [SE-0444: Member import visibility](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0444-member-import-visibility.md)

