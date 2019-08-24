// RUN: not %target-swift-frontend -typecheck -primary-file %s

func hexEncodeBytes<T: Collection>(_ bytes: T) where T.Generator.Element == UInt8 { }
