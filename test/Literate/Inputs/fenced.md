Fenced Code Blocks
==================

A simple fenced code block:

```
// CHECK: SIMPLE
print("SIMPLE")
```

Fenced code blocks can use tildes instead of backticks:

~~~
// CHECK: TILDE
print("TILDE")
~~~

This is not a valid code block:

``` print("NOT VALID") // CHECK-NOT: NOT_VALID ```

This *is* a valid code block:

~~~ swift ```
// CHECK: BACKTICKS IN INFO
print("BACKTICKS IN INFO")
~~~

Fenced code blocks do *not* need a blank line before:
```
// CHECK: NO BLANK REQUIRED
print("NO BLANK REQUIRED")
```

We ignore fenced blocks for other languages:

```pascal
// CHECK-NOT: IGNORED OTHER LANGUAGE
print("IGNORED OTHER LANGUAGE")
```

We process *swift* blocks

```swift
// CHECK: SWIFT BLOCK
print("SWIFT BLOCK")
```

unless they use the `nocompile` keyword:

```swift nocompile
// CHECK-NOT: SWIFT NOCOMPILE
print("SWIFT NOCOMPILE")
```
