struct Borrow<Referent: ~Copyable>: ~Escapable {
	let value: Builtin.Borrow<Referent>
	
	@_lifetime(borrow referent)
	init(_ referent: Referent) {
		self.value = Builtin.makeBorrow(referent)
	}

	var referent: Referent {
		borrow {
			return Builtin.dereferenceBorrow(value)
		}
	}
}
