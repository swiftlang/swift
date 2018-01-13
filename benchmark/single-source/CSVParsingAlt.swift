extension String {
    func parseAlt() -> [[String]] {
        var result: [[String]] = [[]]
        var currentField = "".unicodeScalars
        var inQuotes = false
        
        @inline(__always) func flush() {
            result[result.endIndex-1].append(String(currentField))
            currentField.removeAll()
        }
        
        for c in self.unicodeScalars {
            switch (c, inQuotes) {
            case (",", false):
                flush()
            case ("\n", false):
                flush()
                result.append([])
            case ("\"", _):
                inQuotes = !inQuotes
            default:
                currentField.append(c)
            }
        }
        flush()
        return result
    }
}
