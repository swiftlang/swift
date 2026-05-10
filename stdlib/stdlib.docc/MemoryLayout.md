# ``Swift/MemoryLayout``

## Topics

### Accessing the Layout of a Type

Use these static properties to access a type's layout. For example, the size of a
`Double` instance is `MemoryLayout<Double>.size`.

- ``Swift/MemoryLayout/size``
- ``Swift/MemoryLayout/alignment``
- ``Swift/MemoryLayout/stride``

### Accessing the Layout of a Value

Pass an instance to these static methods to acess the layout for that instance's
type.

- ``Swift/MemoryLayout/stride(ofValue:)``
- ``Swift/MemoryLayout/size(ofValue:)``
- ``Swift/MemoryLayout/alignment(ofValue:)``

### Querying Type Properties

- ``Swift/MemoryLayout/offset(of:)``
