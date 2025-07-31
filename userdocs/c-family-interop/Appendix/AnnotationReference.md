# Annotation Reference

This page provides a summary of the common C attributes and their API Notes equivalents used to refine how Swift imports C APIs. Use this reference to find the right annotation for your specific goal.

### Type Refinement

Use these annotations to change how Swift imports C `struct` and `enum` types.

- term **Swift Enum**: Imports a C `enum` as a native, type-safe Swift `@objc enum` with cases, enabling exhaustive `switch` statements and dot syntax. For more information, see <doc:ImportingCEnumerationsAsSwiftEnums>.
    
    //FIXME: <explaination of issue>
    // According to CToSwiftNameTranslation.md, enum_extensibility imports as @objc enums, not regular Swift enums
    
    - **Clang Attribute:** `__attribute__((enum_extensibility(closed)))`
    - **API Notes:**
      ```yaml
      Tags:
        - Name: YourEnumType
          EnumExtensibility: closed
      ```

- term **Swift OptionSet**: Imports a C `enum` of bitmask flags as a Swift `OptionSet`, enabling idiomatic, set-like operations. For more information, see <doc:ImportingCFlagSetsAsSwiftOptionSets>.
    
    - **Clang Attribute:** `__attribute__((flag_enum))`
    - **API Notes:**
      ```yaml
      Tags:
        - Name: YourFlagsType
          OptionSet: true
      ```

- term **Swift Class**: Imports a reference-counted C struct pointer as a memory-managed Swift `class` by teaching ARC its retain and release functions. For more information, see <doc:ManagingTheLifetimeOfCObjects>.
    
    - **C Macro:** `SWIFT_SHARED_REFERENCE(retainFn, releaseFn)`
    - **API Notes:**
      ```yaml
      Tags:
        - Name: YourStructName_s # The underlying struct, not the typedef
          SwiftImportAs: reference
          SwiftRetainOp: retainFn
          SwiftReleaseOp: releaseFn
      ```

### API Reshaping

Use the `swift_name` attribute to change how a C function is imported, transforming it into a more idiomatic Swift method, property, or initializer.

- term **Instance Method**: Imports a global C function as an instance method on a Swift type. For more information, see <doc:ConvertingGlobalFunctionsToInstanceMethods>.
    
    - **C Macro:** `SWIFT_NAME("Type.method(self:label:)")`
    - **API Notes:**
      ```yaml
      Functions:
        - Name: YourCFunctionName
          SwiftName: "Type.method(self:label:)"
      ```

- term **Computed Property**: Imports a C getter function as a read-only computed property on a Swift type. For more information, see <doc:MappingGetterFunctionsToComputedProperties>.
    
    - **C Macro:** `SWIFT_NAME("getter:Type.property(self:)")`
    - **API Notes:**
      ```yaml
      Functions:
        - Name: YourCFunctionName
          SwiftName: "getter:Type.property(self:)"
      ```

- term **Initializer**: Imports a C factory function as a native Swift initializer on a type. For more information, see <doc:TransformingFactoryFunctionsIntoInitializers>.
    
    - **C Macro:** `SWIFT_NAME("Type.init(label:)")`
    - **API Notes:**
      ```yaml
      Functions:
        - Name: YourCFunctionName
          SwiftName: "Type.init(label:)"
      ```

### Memory, Lifetime, and Nullability Safety

Use these annotations to provide the Swift compiler with crucial information about memory ownership, pointer validity, and nullability, enabling it to generate safer interfaces.

- term **Non-Null Pointer**: Guarantees a C pointer is not null, importing it as a non-optional type in Swift. For more information, see <doc:AnnotatingPointersAndReturnValues>.
    
    - **C Annotation:** `_Nonnull`
    - **API Notes:**
      ```yaml
      # For a parameter or return value
      Nullability: N
      ```

- term **Nullable Pointer**: Indicates a C pointer can be null, importing it as a proper Optional (`?`) in Swift. For more information, see <doc:AnnotatingPointersAndReturnValues>.
    
    - **C Annotation:** `_Nullable`
    - **API Notes:**
      ```yaml
      # For a parameter or return value
      Nullability: S
      ```

- term **Retained Return Value**: Informs Swift that a function returns an object that has already been retained (+1), preventing a double-retain by ARC. For more information, see <doc:ClarifyingOwnershipOfReturnedObjects>.
    
    - **C Macro:** `SWIFT_RETURNS_RETAINED`
    - **API Notes:**
      ```yaml
      Functions:
        - Name: YourCFunctionName
          SwiftReturnOwnership: retained
      ```

- term **Pointer with Element Count**: Links a pointer parameter to another parameter that specifies its size in number of elements. For more information, see <doc:CreatingSafeBuffersFromPointerAndCountPairs>.
    
    - **Clang Attribute:** `__attribute__((__counted_by__(countParam)))`
    - **API Notes:**
      ```yaml
      Parameters:
        - Position: 0 # Index of the pointer parameter
          BoundsSafety:
            Kind: counted_by
            BoundedBy: "countParam" # Name of the count parameter
      ```

- term **Pointer with Byte Size**: Links a pointer parameter to another parameter that specifies its size in bytes. For more information, see <doc:CreatingSafeBuffersFromPointerAndCountPairs>.
    
    - **Clang Attribute:** `__attribute__((__sized_by__(sizeParam)))`
    - **API Notes:**
      ```yaml
      Parameters:
        - Position: 0 # Index of the pointer parameter
          BoundsSafety:
            Kind: sized_by
            BoundedBy: "sizeParam" # Name of the size parameter
      ```

- term **Non-Escaping Pointer**: Guarantees to Swift that a pointer's lifetime will not extend beyond the duration of the function call. For more information, see <doc:GuaranteeingPointerLifetimesForBufferAccess>.
    
    - **Clang Attribute:** `__attribute__((noescape))`
    - **API Notes:**
      ```yaml
      Parameters:
        - Position: 0 # Index of the pointer parameter
          NoEscape: true
      ```
