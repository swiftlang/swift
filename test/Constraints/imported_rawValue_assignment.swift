// RUN: %target-swift-frontend %clang-importer-sdk -typecheck %s -verify
// REQUIRES: objc_interop

import Foundation

class RV {
	init() {
		NSPrefixWordBreak2.breakBarBas.rawValue = 0 // expected-error{{cannot assign to property: 'rawValue' is immutable}}
	}
}
