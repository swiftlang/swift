# ``Swift/fileID()``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Produces a unique identifier for the source file in which the macro appears.

The unique identifier has the form *module*/*file*,
where *file* is the name of the file in which the expression appears
and *module* is the name of the module that this file is part of.

Because `#fileID` doesn't embed the full path to the source file,
unlike `#filePath`,
it gives you better privacy and reduces the size of the compiled binary.

Note: To parse a `#fileID` expression,
read the module name as the text before the first slash (`/`)
and the filename as the text after the last slash.
In future versions of Swift,
the string might contain multiple slashes,
such as `MyModule/some/disambiguation/MyFile.swift`.

This macro's value can be changed by `#sourceLocation`,
as described in [Line Control Statement][] in [The Swift Programming Language][tspl].

[Line Control Statement]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/statements#Line-Control-Statement
[tspl]: https://docs.swift.org/swift-book/

