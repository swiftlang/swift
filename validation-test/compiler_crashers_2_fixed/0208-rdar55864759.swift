// RUN: not %target-swift-frontend -typecheck %s %S/Inputs/0208-rdar55864759-protocol.swift
struct StringInterpolation: MagicStringInterpolationProtocol {}

