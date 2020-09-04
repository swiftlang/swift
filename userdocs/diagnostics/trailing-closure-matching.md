# Argument Matching for Trailing Closures

In Swift, calling a function with one or more trailing closure arguments requires the label of the first trailing closure argument to be omitted. As a result, the compiler must consider additional context when determining which function parameter the trailing closure should satisfy.

Before Swift 5.3, the compiler used a backward scanning rule to match a trailing closure to a function parameter. Starting from the end of the parameter list, it moved backwards until finding a parameter which could accept a trailing closure argument (a function type, unconstrained generic parameter, `Any`, etc.). This could sometimes lead to unexpected behavior. Consider the following example:

```swift
func animate(
  withDuration duration: Double, 
  animations: () -> Void, 
  completion: (() -> Void)? = nil
) {}

// OK
animate(withDuration: 0.3, animations: { /* Animate Something */ }) {
  // Done!
}

// error: missing argument for parameter 'animations' in call
animate(withDuration: 0.3) {
  // Animate Something
}
```

The second call to `animate` results in a compiler error because the backward scanning rule matches the trailing closure to the `completion` parameter instead of `animations`.

Beginning in Swift 5.3, the compiler uses a new, forward scanning rule to match trailing closures to function parameters. When matching function arguments to parameters, the forward scan first matches all non-trailing arguments from left-to-right. Then, it continues matching trailing closures in a left-to-right manner. This leads to more predictable and easy-to-understand behavior in many situations. With the new rule, the example above now works as expected without any modifications:

```swift
// Remains valid
animate(withDuration: 0.3, animations: { /* Animate Something */ }) {
  // Done!
}

// Also OK!
animate(withDuration: 0.3) {
  // Animate Something
}
```

When scanning forwards to match an unlabeled trailing closure argument, the compiler may sometimes need to "skip over" defaulted and variadic arguments. The new rule will skip any parameter that does not structurally resemble a function type. This allows writing a modified version of the above example where `withDuration` also has a default argument value:

```swift
func animate(
  withDuration duration: Double = 1.0, 
  animations: () -> Void, 
  completion: (() -> Void)? = nil
) {}

// Allowed! The forward scanning rule skips `withDuration` because it does not
// structurally resemble a function type.
animate {
  // Animate Something
}
```

A parameter structurally resembles a function type if both of the following are true:

- The parameter is not `inout`
- The adjusted type of the parameter is a function type

The adjusted type of the parameter is the parameter's type as it appears in the function declaration, looking through any type aliases, and performing three additional adjustments:

- If the parameter is an `@autoclosure`, using the result type of the parameter's declared (function) type, before performing the second adjustment.
- If the parameter is variadic, looking at the base element type.
- Removing all outer "optional" types.

To maintain source compatibility with code that was written before Swift 5.3, the forward scanning rule applies an additional heuristic when matching trailing closure arguments. If,

- the parameter that would match an unlabeled trailing closure argument according to the forward scanning rule does not require an argument (because it is variadic or has a default argument), _and_
- there are parameters _following_ that parameter that _do_ require an argument, which appear before the first parameter whose label matches that of the _next_ trailing closure (if any)

then the compiler does not match the unlabeled trailing closure to that parameter. Instead, it skips it and examines the next parameter to see if that should be matched against the unlabeled trailing closure. This can be seen in the following example:

```swift
func showAlert(
  message: String,
  onPresentation: (() ->  Void)? = nil,
  onDismissal: () -> Void
) {}

// The unlabeled trailing closure matches `onDismissal` because `onPresentation`
// does not require an argument, but `onDismissal` does and there are no other
// trailing closures which could match it.
showAlert(message: "Hello, World!") {
  // On dismissal action
}

// The unlabeled trailing closure matches `onPresentation` because although
// `onPresentation` does not require an argument, there are no parameters
// following it which require an argument and appear before the parameter
// whose label matches the next trailing closure argument (`onDismissal`).
showAlert(message: "Hello, World!") {
  // On presentation action
} onDismissal: {
  // On dismissal action
}
```

Additionally, the Swift 5 compiler will attempt to apply both the new forward scanning rule and the old backward scanning rule when it encounters a call with a single trailing closure. If the forward and backward scans produce *different* valid assignments of arguments to parameters, the compiler will prefer the result of the backward scanning rule and produce a warning.

To learn more about argument matching for trailing closures, see [Swift Evolution Proposal SE-0286](https://github.com/apple/swift-evolution/blob/master/proposals/0286-forward-scan-trailing-closures.md).
