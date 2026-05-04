# Debugging and Reflection

Fortify your code with runtime checks, and examine your values' runtime representation.

## Topics

### Printing and Dumping

- ``Swift/print(_:separator:terminator:)``
- ``Swift/print(_:separator:terminator:to:)``
- ``Swift/debugPrint(_:separator:terminator:)``
- ``Swift/debugPrint(_:separator:terminator:to:)``
- ``Swift/dump(_:name:indent:maxDepth:maxItems:)``
- ``Swift/dump(_:to:name:indent:maxDepth:maxItems:)``

### Testing

- ``Swift/assert(_:_:file:line:)``
- ``Swift/assertionFailure(_:file:line:)``
- ``Swift/precondition(_:_:file:line:)``
- ``Swift/preconditionFailure(_:file:line:)``

### Exiting a Program

- ``Swift/fatalError(_:file:line:)``
- ``Swift/Never``

### Querying Runtime Values

- ``Swift/Mirror``
- ``Swift/ObjectIdentifier``
- ``Swift/type(of:)``
- ``Swift/==(_:_:)-(,_)``
- ``Swift/!=(_:_:)-(,_)``

### Customizing Your Type's Reflection

Provide a custom reflection for your types using these protocols.

- ``Swift/CustomReflectable``
- ``Swift/CustomLeafReflectable``
- ``Swift/CustomPlaygroundDisplayConvertible``
- ``Swift/PlaygroundQuickLook``
- ``Swift/DebugDescription()``
