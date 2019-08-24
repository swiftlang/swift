extension Thing : Equatable {}
func == (a: Thing, b: Thing) -> Bool { return a.value == b.value }

