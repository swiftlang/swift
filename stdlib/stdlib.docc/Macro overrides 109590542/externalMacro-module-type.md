# ``Swift/externalMacro(module:type:)``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Specifies the module and type name for a macro's implementation.

This macro can only be used to define a macro;
using it in any other context is an error.
The specified type must conform to the protocols
that correspond to the roles of the macro being declared.
For example:

```swift
macro stringify(_ value: T) -> (T, String) =
    #externalMacro(module: "ExampleMacros", type: "StringifyMacro")
```


- Parameters:
    - module: The module name.
    - type: The type that implements the macro.

- Returns: The macro's implementation.

