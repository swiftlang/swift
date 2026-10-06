# Property Wrapper Support for Variadic Parameters

## Summary
This PR adds support for applying property wrappers to variadic parameters in Swift. Previously, property wrappers could not be correctly applied to variadic parameters, limiting their usefulness in certain scenarios.

## Motivation
Property wrappers are a powerful feature in Swift, but their inability to work with variadic parameters creates an inconsistency in the language. This implementation allows developers to use property wrappers with variadic parameters, enabling more consistent and expressive APIs.

## Proposed solution
The solution modifies the type checker to properly handle property wrappers when applied to variadic parameters. When a property wrapper is applied to a variadic parameter:
1. The wrapper is applied to the array type that represents the variadic parameter
2. The compiler ensures proper type checking and validation
3. The wrapped value maintains the variadic parameter's semantics

## Impact on existing code
This change is purely additive and does not affect existing code. Code that previously didn't compile due to property wrappers on variadic parameters will now work as expected.

## Implementation details
- Added `applyPropertyWrapperToVariadicParam` to handle variadic parameter wrapping
- Modified `validateParameterPropertyWrapper` to support variadic parameters
- Added comprehensive tests to verify the functionality
- Ensured proper type checking and validation

## Testing
Added tests that verify:
- Basic property wrapper functionality with variadic parameters
- Custom initialization of property wrappers on variadic parameters
- Edge cases and error conditions

## Future directions
- Consider adding support for projected values on variadic parameters
- Explore optimizations for the implementation
- Add more built-in property wrappers that work well with variadic parameters

## Source compatibility
This change is purely additive and maintains source compatibility.

## Effect on ABI stability
No impact on ABI stability as this is purely additive.

## Related issues
Fixes #[XXX-XXX]
