# ``Swift/filePath()``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Produces the complete path to the file in which the macro appears.

Because `#fileID` doesn't embed the full path to the source file,
unlike `#filePath`,
it gives you better privacy and reduces the size of the compiled binary.
Avoid using `#filePath` outside of tests, build scripts,
or other code that doesn't become part of the shipping program.

This macro's value can be changed by `#sourceLocation`,
as described in [Line Control Statement][] in [The Swift Programming Language][tspl].

[Line Control Statement]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/statements#Line-Control-Statement
[tspl]: https://docs.swift.org/swift-book/
