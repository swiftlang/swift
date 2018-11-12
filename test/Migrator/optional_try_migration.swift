// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -c -update-code -swift-version 4 -primary-file %s -emit-migrated-file-path %t/optional_try_migration.result.swift -o %t/rename-func-decl.swift.remap
// RUN: diff -u %S/optional_try_migration.swift.expected %t/optional_try_migration.result.swift

func throwOrOptional() throws -> Int? {
    return 3
}

func throwOrInt() throws -> Int {
    return 3
}

func doubleOptional() -> Int?? {
    return 3
}

func testIfLet() {
    if let optionalX = try? throwOrOptional(), // This comment should be preserved
        let y = Optional(3), // This will stay here
        let x = optionalX // this comment will be moved
    { 
        print(x, y)
        return 
    }

    if let optionalX = doubleOptional(),
       let x = optionalX 
    {
        // This one shouldn't be changed
        print(x)
    }

    if let x = try? throwOrInt(),
       let y = try? throwOrInt() {
        // This one shouldn't be changed
        print(x, y)
    }

    if let optionalX = try? throwOrOptional(),
       let optionalY = try? throwOrOptional(),
       let x = optionalX,
       let y = optionalY
    {
        print(x, y)
    }
}
