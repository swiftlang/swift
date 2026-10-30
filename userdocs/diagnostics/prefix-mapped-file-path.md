# Prefix-mapped file paths (PrefixMappedFilePath)

When compilation caching is enabled with path prefix mapping (`-scanner-prefix-map`), the compiler sees each source file at a canonical path such as `/^src/Tests/AppTests/ParserTests.swift` rather than its real location, so that the same cached result can be reused across different checkouts. `#filePath`, and `#file` in Swift 5 mode, evaluate to this prefix-mapped path.

Code that uses the result only to identify the source file in messages keeps working, but code that uses it to access the file system, such as locating test resources next to the source file, will fail because no file exists at that path.

The `PrefixMappedFilePath` diagnostic group is disabled by default, but can be enabled with `-Wwarning PrefixMappedFilePath` to identify each `#filePath` that evaluates to a prefix-mapped path. For example:

```swift
// Compiled from /^src/Tests/AppTests/ParserTests.swift
let resources = URL(fileURLWithPath: #filePath)
  .deletingLastPathComponent()
  .appendingPathComponent("Resources")
// warning: '#filePath' evaluates to the prefix-mapped path '/^src/Tests/AppTests/ParserTests.swift', not the original path of the source file
```

Only literals written in source are diagnosed: a `#filePath` default argument is diagnosed at the parameter declaration, not at each call that uses the default.

To resolve the warning, locate files in a way that doesn't depend on the path of the source file, such as by bundling them as resources of the module, or exclude the source directory from prefix mapping. If the path only identifies the source file in messages, consider using `#fileID` instead.
