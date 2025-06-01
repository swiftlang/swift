# Member import visibility


This diagnostic group includes the errors that are emitted when a member declaration cannot be accessed because the module defining that member has not been directly imported by the file containing the reference:

```
func getFileContents() throws -> String {
  // error: initializer 'init(contentsOfFile:)' is not available due to missing import of defining module 'Foundation'
  return try String(contentsOfFile: "example.txt")
}
```

To resolve the error, you must add an import of the module that defines the member.

These errors are only emitted when the `MemberImportVisibility` language mode is enabled.
