# Basic Behaviors

Use your custom types in operations that depend on testing for equality or order
and as members of sets and dictionaries.

## Topics

### Equality and Ordering

Allow your custom types to be used with simple collection operations, such as `contains(_:)`,
and standard comparison operators.

- ``Swift/Equatable``
- ``Swift/Comparable``
- ``Swift/Identifiable``

### Copying

- ``Swift/Copyable``
- ``Swift/BitwiseCopyable``
- ``Swift/Escapable``

### Sets and Dictionaries

Store your custom types in sets or use them as dictionary keys.

- ``Swift/Hashable``
- ``Swift/Hasher``

### String Representation

- ``Swift/CustomStringConvertible``
- ``Swift/LosslessStringConvertible``
- ``Swift/CustomDebugStringConvertible``

### Raw Representation

- ``Swift/CaseIterable``
- ``Swift/RawRepresentable``
