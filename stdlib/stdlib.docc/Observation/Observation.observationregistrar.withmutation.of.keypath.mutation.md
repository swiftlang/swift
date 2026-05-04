# ``Observation/ObservationRegistrar/withMutation(of:keyPath:_:)``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Identifies mutations to the transactions registered for observers.

This method calls ``willSet(_:keyPath:)`` before the mutation. Then it
calls ``didSet(_:keyPath:)`` after the mutation.

- Parameters:
	- subject: An instance of an observable type.
	- keyPath: The key path of an observed property.
  - mutation: A closure that provides the mutation to apply.
