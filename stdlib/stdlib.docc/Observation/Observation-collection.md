# Observation

Make responsive apps that update the presentation when underlying data changes.

## Overview

Observation provides a robust, type-safe, and performant implementation of the
observer design pattern in Swift. This pattern allows an observable object to 
maintain a list of observers and notify them of specific or general state 
changes. This has the advantages of not directly coupling objects together and 
allowing implicit distribution of updates across potential multiple observers. 

The Observation frameworks provides the following capabilities:

- Marking a type as observable
- Tracking changes within an instance of an observable type
- Observing and utilizing those changes elsewhere, such as in an app's user 
interface

To declare a type as observable, attach the ``Observation/Observable()`` macro 
to the type declaration. This macro declares and implements conformance to the 
``Observation/Observable`` protocol to the type at compile time. 

```swift
@Observable
class Car {
    var name: String = ""
    var needsRepairs: Bool = false
    
    init(name: String, needsRepairs: Bool = false) {
        self.name = name
        self.needsRepairs = needsRepairs
    }
}
```

To track changes, use the ``Observation/withObservationTracking(_:onChange:)`` function.
For example, in the following code, the function calls the `onChange` closure
when a car's name changes. However, it doesn't call the closure when a car's 
`needsRepair` flag changes. That's because the function only tracks properties
read in its `apply` closure, and the closure doesn't read the `needsRepair`
property.

    func render() {
        withObservationTracking {
            for car in cars {
                print(car.name)
            }
        } onChange: {
            print("Schedule renderer.")
        }
    }

## Topics

### Observable conformance

- ``Observation/Observable()``
- ``Observation/Observable``

### Fine-tuning

- ``Observation/ObservationIgnored()``
- ``Observation/ObservationTracked()``

### Change tracking

- ``Observation/withObservationTracking(_:onChange:)``
- ``Observation/ObservationRegistrar``

