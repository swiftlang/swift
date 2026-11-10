// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-experimental-feature ClassAllocators -target %target-future-triple -module-name Allocators -emit-module-path %t/Allocators.swiftmodule %S/Inputs/class_allocators.swift
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ClassAllocators -target %target-future-triple -I %t -default-class-allocator Allocators.Pool

// REQUIRES: swift_feature_ClassAllocators

// The module imports Allocators implicitly: `Pool` resolves without an import.

// -default-class-allocator applies to every class that has no allocator.
class Defaulted {}
final class DefaultedSubclass: Defaulted {}

// `@allocator` overrides it, and the nearest declaration wins.
@allocator(OtherPool)
class Declared {}
final class InheritsDeclared: Declared {}

@allocator(SystemClassAllocator)
class OptedOut {}
final class InheritsOptOut: OptedOut {}

// Actors get it like any class, and may opt out.
actor DefaultedActor {}

@allocator(SystemClassAllocator)
actor OptedOutActor {}

// Classes that can't have an allocator must opt out.
final class Buffer: ManagedBuffer<Int, Int> {} // expected-error {{default class allocator 'Pool' cannot be applied to class 'Buffer' because it is a subclass of 'ManagedBuffer'}}
// expected-note@-1 {{add '@allocator(SystemClassAllocator)' to use the system allocator}} {{1-1=@allocator(SystemClassAllocator) }}

@allocator(SystemClassAllocator)
class OptedOutBuffer: ManagedBuffer<Int, Int> {}
final class InheritsOptedOutBuffer: OptedOutBuffer {}

// The allocator named by the flag is visible to clients: it's public.
public class PublicDefaulted {}

func use() {
  _ = Pool.self
}
