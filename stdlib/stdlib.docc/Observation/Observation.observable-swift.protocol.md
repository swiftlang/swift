# ``Observation/Observable``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

A type that emits notifications to observers when underlying data changes.

Conforming to this protocol signals to other APIs that the type supports
observation. However, applying the `Observable` protocol by itself to a
type doesn't add observation functionality to the type. Instead, always use
the ``Observation/Observable()`` macro when adding observation
support to a type.
