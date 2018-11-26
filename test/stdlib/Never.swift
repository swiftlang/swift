// RUN: %target-typecheck-verify-swift

struct ConformsToError<T: Error> {}
_ = ConformsToError<Never>()

struct ConformsToEquatable<T: Equatable> {}
_ = ConformsToEquatable<Never>()

struct ConformsToComparable<T: Comparable> {}
_ = ConformsToComparable<Never>()

struct ConformsToHashable<T: Hashable> {}
_ = ConformsToHashable<Never>()
