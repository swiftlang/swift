// RUN: %target-swift-frontend -target %target-future-triple -parse-as-library -emit-silgen %s

// Also check if there's any weird accidental interactions with modes which affect default isolation:
// RUN: %target-swift-frontend -target %target-future-triple -parse-as-library -emit-silgen %s -enable-upcoming-feature NonisolatedNonsendingByDefault
// RUN: %target-swift-frontend -target %target-future-triple -parse-as-library -emit-silgen %s -enable-upcoming-feature NonisolatedNonsendingByDefault -default-isolation=MainActor

// REQUIRES: concurrency
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// Isolated deinit was wrongly passed generic substitution map in SIL, which would then crash.

@MainActor
final class GenericMainActorClass<T> {
  isolated deinit {}
}

actor GenericActor<T> {
  isolated deinit {}
}
