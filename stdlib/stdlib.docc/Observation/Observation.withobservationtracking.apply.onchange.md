# ``Observation/withObservationTracking(_:onChange:)``

@Metadata {
    @DocumentationExtension(mergeBehavior: override)
}

Tracks access to properties.

This method tracks access to any property within the `apply` closure, and 
informs the caller of value changes made to participating properties by way of
the `onChange` closure. For example, the following code tracks changes to the
name of cars, but it doesn't track changes to any other property of `Car`:

    func render() {
        withObservationTracking {
            for car in cars {
                print(car.name)
            }
        } onChange: {
            print("Schedule renderer.")
        }
    }

- Parameters:
    - apply: A closure that contains properties to track.
    - onChange: The closure invoked when the value of a property changes. 

- Returns: The value that the `apply` closure returns if it has a return value; otherwise, there is no return value.
