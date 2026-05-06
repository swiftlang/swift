# ``Swift/function()``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Produces the name of the declaration in which it appears.

Inside a function,
the value `#function` produces is the name of that function,
inside a method it's the name of that method,
inside a property getter or setter it's the name of that property,
inside special members like `init` or `subscript`
it's the name of that keyword,
and at the top level of a file it's the name of the current module.

When used as the default value of a function or method parameter,
this macro's value is determined
when the default value expression is evaluated at the call site.
For example:

```swift
func logFunctionName(string: String = #function) {
    print(string)
}
func myFunction() {
    logFunctionName() // Prints "myFunction()".
}
```


