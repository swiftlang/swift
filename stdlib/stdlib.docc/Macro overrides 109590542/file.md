# ``Swift/file()``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Produces the path to the file in which it appears.

The string value from `#file` depends on the language version,
to enable migration from the old `#filePath` behavior
to the new `#fileID` behavior.
Currently, `#file` has the same value as `#filePath`.
In a future version of Swift,
`#file` will have the same value as `#fileID` instead.
To adopt the future behavior,
replace `#file` with `#fileID` or `#filePath` as appropriate.

This macro's value can be changed by `#sourceLocation`,
as described in [Line Control Statement][] in [The Swift Programming Language][tspl].

[Line Control Statement]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/statements#Line-Control-Statement
[tspl]: https://docs.swift.org/swift-book/
