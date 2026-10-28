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

An override of a method, initializer, or subscript, or a witness for an `@objc` protocol requirement, can use a non-optional type for a parameter that the overridden declaration or requirement declares as an implicitly unwrapped optional. This often happens with Objective-C APIs that lack nullability annotations. A caller can still pass `nil` to that parameter: calls from Swift will force-unwrap it on entry, and `nil` from Objective-C callers isn't checked at all. The same applies to a witness of non-optional type for a settable property requirement whose type is implicitly unwrapped, because `nil` can be assigned to it. For example, given this Objective-C class and protocol:

```objc
@interface Base : NSObject
- (void)update:(NSObject *)object;
@end

@protocol Model
@property NSObject *value;
@end
```

these declarations are diagnosed:

```swift
class Derived: Base {
  override func update(_ object: NSObject) { } // warning: implicit force unwrap of implicitly unwrapped optional parameter of type 'NSObject?' in override of instance method 'update'
}

class MyModel: NSObject, Model {
  var value: NSObject = NSObject() // warning: implicit force unwrap of implicitly unwrapped optional value of type 'NSObject?' in witness for property 'value'
}
```

The Fix-Its change the types to `NSObject!`, so that uses of the parameter or property are force-unwrapped (and diagnosed) explicitly. Alternatively, use the optional type `NSObject?` and handle `nil`. Result types are not diagnosed, because returning a non-optional value where an implicitly unwrapped optional is expected is always safe.
