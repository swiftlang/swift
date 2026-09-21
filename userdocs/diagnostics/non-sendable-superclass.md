# Unable to conform to Sendable with a non-Sendable superclass (NonSendableSuperclass)

## Overview

A class cannot conform to `Sendable` when it inherits from a non-Sendable superclass. This prohibition exists because the superclass is free to change its exposed API or internal implementation in ways that would make a `Sendable` conformance unsafe. Consider code like this:

```swift
class MIDIDevice {}

// ...

final class MIDISynth: MIDIDevice, Sendable {}
//          |          `- note: inherits from non-Sendable class 'MIDIDevice'
//          |- error: class 'MIDISynth' cannot conform to the 'Sendable' protocol
//          `- note: a 'Sendable' class cannot inherit from a non-Sendable class
```

Since `MIDIDevice` isn't `Sendable`, it is allowed to expose mutable state:

```swift
class MIDIDevice {
  var activeNotes: [Note] = []
}
```

`MIDISynth` would inherit `activeNotes`, and treating `MIDISynth` as `Sendable` would allow `activeNotes` to be mutated concurrently, risking a data race.

Normally, a global actor like `@MainActor` implies `Sendable`, but this is not the case for classes with non-Sendable superclasses for the same reason.

```swift
@MainActor
class MIDISynth: MIDIDevice {} // Not implicitly Sendable!
```

If `@MainActor` made `MIDISynth` conform to `Sendable`, `MIDISynth` could be sent to multiple actors, upcast to `MIDIDevice`, and concurrently mutated:

```swift
@MainActor
func useSynth(_ synth: MIDISynth) {
  Task.detached { // sending would only be possible if MIDISynth were Sendable
    let device: MIDIDevice = synth  // upcast strips the @MainActor isolation
    device.activeNotes.append(.A)   // mutates off the main actor...
  }
  synth.activeNotes.append(.C)      // ...racing with the detached task's mutation
}
```

However, since `MIDISynth` is *not* allowed to conform to `Sendable`, sending it to a task while still referencing it on the main actor will be an error.

## How to fix

If you need to send a class that inherits from a non-Sendable superclass, you have a few options:

- If you wrote the superclass, isolate the superclass to a global actor. Its state is then actor-protected, the base becomes implicitly `Sendable`, and the subclass inherits that conformance:

  ```swift
  @MainActor
  class MIDIDevice {
    var activeNotes: [Note] = []
  }

  class MIDISynth: MIDIDevice {} // inherits Sendable and @MainActor from MIDIDevice
  ```

- If you don't control the superclass, you may conform with `@unchecked Sendable` if you guarantee the safety of the inherited and subclass state yourself:

  ```swift
  class MIDISynth: MIDIDevice, @unchecked Sendable {}
  ```

- This option also applies to a global-actor-isolated subclass, but be aware that your subclass can be upcast after being sent, so actor isolation is insufficient to protect the inherited state:

  ```swift
  @MainActor
  class MIDISynth: MIDIDevice, @unchecked Sendable {}
  ```

- Instead of trying to conform to `Sendable`, store the instance of the class that you need to share inside a synchronized container, like `Mutex`:

  ```swift
  import Synchronization

  let synth = Mutex(MIDISynth())

  Task.detached {
    synth.withLock { $0.activeNotes.append(.A) }
  }
  ```

## `NSObject`

An exception is made for directly inheriting from `NSObject`. `NSObject` does not and will not expose mutable stored properties, so direct subclasses are allowed to conform to `Sendable` for Objective-C interoperability.

## See Also

- [Unavailable Sendable conformance (UnavailableSendableConformance)](unavailable-sendable-conformance.md)
- [SE-0434: Usability of global-actor-isolated types](https://github.com/swiftlang/swift-evolution/blob/main/proposals/0434-global-actor-isolated-types-usability.md)
- [Sendable Types](https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/#Sendable-Types)
- [Explicit `Sendable` annotations on public type declarations (ExplicitSendable)](explicit-sendable-annotations.md)
