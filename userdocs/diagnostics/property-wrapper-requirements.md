# Property Wrapper Implementation Requirements
If a type is marked with the `@propertyWrapper` attribute, it must meet certain requirements to be a valid property wrapper.

First, all property wrapper types must have a property named `wrappedValue`. This property cannot be static and must have the same access level as the property wrapper type. If the property wrapper provides a `projectedValue` property, it is subject to the same requirements.

Second, none of a property wrapper's initializers may be failable. Additionally, if a property wrapper initializer has a `wrappedValue` parameter, the type of that parameter must either be the same as the type of the `wrappedValue` property or an `@autoclosure` of that type.