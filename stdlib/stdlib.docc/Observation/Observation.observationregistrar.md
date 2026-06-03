# ``Observation/ObservationRegistrar``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Provides storage for tracking and access to data changes.

You don't need to create an instance of `ObservationRegistrar` when using the
``Observation/Observable()`` macro to indicate observability of a
type.

## Topics

### Creating an observation registrar

- ``Observation/ObservationRegistrar/init()``

### Receiving change notifications

- ``Observation/ObservationRegistrar/willSet(_:keyPath:)``
- ``Observation/ObservationRegistrar/didSet(_:keyPath:)``

### Identifying transactional access

- ``Observation/ObservationRegistrar/access(_:keyPath:)``
- ``Observation/ObservationRegistrar/withMutation(of:keyPath:_:)``
