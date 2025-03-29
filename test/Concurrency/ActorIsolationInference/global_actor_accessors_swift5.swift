// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -parse-as-library -verify-additional-prefix swift5- %S/Inputs/global_actor_accessors.swift

// REQUIRES: concurrency
