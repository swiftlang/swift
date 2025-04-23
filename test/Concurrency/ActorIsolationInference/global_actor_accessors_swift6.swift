// RUN: %target-swift-frontend -typecheck -verify -swift-version 6 -parse-as-library -verify-additional-prefix swift6- -verify-additional-prefix swift6+- %S/Inputs/global_actor_accessors.swift

// REQUIRES: concurrency
