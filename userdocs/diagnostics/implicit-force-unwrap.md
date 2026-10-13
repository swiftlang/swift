# Implicit force unwrap (ImplicitForceUnwrap)

Swift will implicitly force-unwrap an optional value in a few places, trapping at runtime if the value is `nil`:

* A reference to a declaration with an implicitly unwrapped optional type, such as a property declared as `Int!` or a function that returns `String!`, is force-unwrapped when it is used where a non-optional value is required.
* The result of a dynamic member lookup on `AnyObject`, such as calling an `@objc` method through a value of type `AnyObject`, is force-unwrapped because the member might not exist at runtime.

The `ImplicitForceUnwrap` diagnostic group is disabled by default, but can be enabled to identify each place where such an implicit force unwrap happens. Each warning comes with a Fix-It to insert an explicit `!`, which makes the potential runtime failure visible in source. For example:

```swift
class ViewController {
  var label: Label!

  func update(text: String) {
    label.text = text // warning: implicit force unwrap of implicitly unwrapped optional of type 'Label?'
  }
}

@objc class Model {
  @objc func refresh() -> Bool { ... }
}

func refresh(_ object: AnyObject) {
  _ = object.refresh() // warning: implicit force unwrap of dynamic lookup result of type '(() -> Bool)?'
}
```

Key path components that implicitly force-unwrap a value are diagnosed the same way: `\ViewController.label.text` will warn, and the Fix-It produces `\ViewController.label!.text`.
