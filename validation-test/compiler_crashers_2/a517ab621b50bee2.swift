// {"signature":"swift::GenericEnvironment::getOrCreateArchetypeFromInterfaceType(swift::Type)"}
// RUN: not --crash %target-swift-frontend -typecheck %s
@resultBuilder enum a < b { struct c{@a d:
