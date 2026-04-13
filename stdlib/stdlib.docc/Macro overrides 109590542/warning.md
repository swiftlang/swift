# ``Swift/warning(_:)``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Produces the given warning message during compilation.

Compilation proceeds after emitting the message as a nonfatal warning.

<!--
TR: This was true for the old special-literal form, but seems not to be true now.

You write the diagnostic message as a static string literal.
Static string literals can't use features like
string interpolation or concatenation,
but they can use the multiline string literal syntax.
-->

- Parameter warning: The diagnostic message.

