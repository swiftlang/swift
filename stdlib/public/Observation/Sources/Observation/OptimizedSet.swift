//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

// Observation favors sets with either 1 or 0 elements; this prevents some of 
// the extra traffic of retains/releases and hashes by specializing the
// storage to a enumeration for holding those values and falling back to a 
// standard Set.

enum OptimizedSet<Element: Hashable> {
	case empty
	case single(Element)
	case many(Set<Element>)
}

extension OptimizedSet {
	var isEmpty: Bool {
		switch self {
		case .empty: return true
		case .single: return false
		case .many(let elements): return elements.isEmpty
		}
	}

	mutating func insert(_ element: Element) {
		switch self {
		case .empty: 
			self = .single(element)
		case .single(let existing):
			guard element != existing else {
				return
			}
			self = .many([existing, element])
		case .many(var existing):
			self = .empty // avoid CoW
			existing.insert(element)
			self = .many(existing)
		}
	}

	mutating func remove(_ element: Element) {
		switch self {
		case .empty:
			break
		case .single(let existing):
			if existing == element {
				self = .empty
			}
		case .many(var existing):
			self = .empty
			existing.remove(element)
			let count = existing.count
			if count == 1 {
				self = .single(existing.first!)
			} else {
				self = .many(existing)
			}
		}
	}

	func union(_ other: OptimizedSet<Element>) -> OptimizedSet<Element> {
		switch (self, other) {
		case (.empty, .empty): return .empty
		case (.single(let lhsElement), .empty): return .single(lhsElement)
		case (.empty, .single(let rhsElement)): return .single(rhsElement)
		case (.single(let lhsElement), .single(let rhsElement)):
			if lhsElement == rhsElement {
				return .single(lhsElement)
			} else {
				return .many([lhsElement, rhsElement])
			}
		case (.many(let lhsElements), .empty): return .many(lhsElements)
		case (.empty, .many(let rhsElements)): return .many(rhsElements)
		case (.single(let lhsElement), .many(var rhsElements)):
			rhsElements.insert(lhsElement)
			return .many(rhsElements)
		case (.many(var lhsElements), .single(let rhsElement)):
			lhsElements.insert(rhsElement)
			return .many(lhsElements)
		case (.many(let lhsElements), .many(let rhsElements)):
			return .many(lhsElements.union(rhsElements))
		}
	}
}

extension OptimizedSet: Sequence {
	enum Iterator: IteratorProtocol {
		case empty
		case single(Element)
		case many(Set<Element>.Iterator)

		mutating func next() -> Element? {
			switch self {
			case .empty: return nil
			case .single(let element):
				self = .empty
				return element
			case .many(var iterator):
				self = .empty
				guard let element = iterator.next() else {
					return nil
				}
				self = .many(iterator)
				return element
			}
		}
	}

	func makeIterator() -> Iterator {
		switch self {
		case .empty: .empty
		case .single(let element): .single(element)
		case .many(let elements): .many(elements.makeIterator())
		}
	}
}
