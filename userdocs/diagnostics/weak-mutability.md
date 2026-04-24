# Redundantly mutable weak variables (WeakMutability)

Warnings that identify `weak` variables that are never updated.

## Overview

This is the `weak var` equivalent of the always-on warning for `var`s that are never mutated. Note that replacing `weak var` with `let` makes it a strong reference. This may cause a memory leak in cases where `weak` is actually appropriate, so replacing `weak var` with `let` is not always the correct course of action. There is no such thing as `weak let`, since a weak reference is automatically reassigned to `nil` when the referenced object is deallocated.
