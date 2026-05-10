# ``Observation/ObservationIgnored()``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Disables observation tracking of a property.

By default, an object can observe any property of an observable type that
is accessible to the observing object. To prevent observation of an
accessible property, attach the `ObservationIgnored` macro to the property.
