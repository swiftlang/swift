# ``Swift/error(_:)``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Emits the given message as a fatal error
and terminates the compilation process.

<!--
TR: This was true for the old special-literal form, but seems not to be true now.

You write the diagnostic message as a static string literal.
Static string literals can't use features like
string interpolation or concatenation,
but they can use the multiline string literal syntax.
-->

- Parameter message: The error message.
