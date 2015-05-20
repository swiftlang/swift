// RUN: %target-swift-frontend %clang-importer-sdk -parse %s -verify
// REQUIRES: objc_interop

import Foundation

class RV {
	init() {
		NSPrefixWordBreak2.BreakBarBas.rawValue = 0 // expected-error{{cannot assign to a get-only property 'rawValue'}}
	}
}
