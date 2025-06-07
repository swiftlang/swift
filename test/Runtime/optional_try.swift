// RUN: %target-run-simple-swift -sil-verify-all | %FileCheck %s
// REQUIRES: executable_test

enum Bad: Error {
    case err
    case custom(String)
}

func erase<T>(_ val: T) -> Any {
    return val as Any
}

class Klass {}

typealias MaybeString = Result<String, Error>
typealias MaybeKlass = Result<Klass, Error>
typealias MaybeInt = Result<Int, Error>
typealias MaybeNumbers = Result<[Int], Error>

////////
// NOTE: Do _not_ heed the warnings about implicit coercions to Any.
//       That's an important part of this test's coverage!
////////
// -- throwing --
// CHECK: nil
print( try? MaybeString.failure(Bad.err).get() )

// CHECK: nil
print( try? MaybeKlass.failure(Bad.custom("doggo")).get() )

// CHECK: nil
print( try? MaybeInt.failure(Bad.err).get() )

// CHECK: nil
print( try? MaybeNumbers.failure(Bad.err).get() )

// CHECK: nil
print(erase( try? MaybeNumbers.failure(Bad.err).get() ))

// -- normal --
// CHECK: Optional("catto")
print( try? MaybeString.success("catto").get() )

// CHECK: Optional(main.Klass)
print( try? MaybeKlass.success(Klass()).get() )

// CHECK: Optional(3)
print( try? MaybeInt.success(3).get() )

// CHECK: Optional([4, 8, 15, 16, 23, 42])
print( try? MaybeNumbers.success([4, 8, 15, 16, 23, 42]).get() )

// CHECK: Optional([0, 1, 1, 2, 3])
print(erase( try? MaybeNumbers.success([0, 1, 1, 2, 3]).get() ))
