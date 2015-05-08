// RUN: %target-run-simple-swift | FileCheck %s

// REQUIRES: objc_interop

import Foundation

func Glob(g: String) -> Array<String> {
	var gt = glob_t()
	var rv = glob_b((g as NSString).UTF8String, 0, nil, &gt)

	if rv == 0 {
		var result = Array<String>()
		for x in 0..<Int(gt.gl_pathc) {
			var str = String.fromCString(gt.gl_pathv[x])
			result.append(str!)
		}
		globfree(&gt)
		return result
	}
	return []
}

// CHECK: [{{.*}}]
print(Glob("/*"))
