// RUN: %target-swift-frontend  -parse-as-library -emit-silgen %s | %FileCheck %s

// Check that we don't crash if a local function references another local
// function without captures.

// CHECK-LABEL: sil hidden @_TF14local_captures10globalfuncFT_FT_T_ : $@convention(thin) () -> @owned @callee_owned () -> ()
func globalfunc() -> () -> () {

	// CHECK-LABEL: sil shared @_TFF14local_captures10globalfuncFT_FT_T_L_9localFuncFT_T_ : $@convention(thin) () -> ()
	func localFunc() {
	}

	// CHECK-LABEL: sil shared @_TFF14local_captures10globalfuncFT_FT_T_L_6callitFT_T_ : $@convention(thin) () -> ()
	func callit() {
		localFunc()
	}

	// CHECK-LABEL: sil shared @_TFF14local_captures10globalfuncFT_FT_T_L_5getitFT_FT_T_ : $@convention(thin) () -> @owned @callee_owned () -> ()
	func getit() -> () -> () {
		return localFunc
	}

	callit()
	return getit()
}
