#pragma once

#include <string>
#include <vector>
#include <memory>
#include "llvm/Support/raw_ostream.h"

namespace swiftc {

/// Diagnostic levels
enum class DiagnosticLevel {
    Note,
    Warning,
    Error,
    Fatal
};

/// Individual diagnostic message
struct Diagnostic {
    DiagnosticLevel level;
    std::string message;
    std::string file;
    unsigned line;
    unsigned column;
    
    Diagnostic(DiagnosticLevel lvl, const std::string& msg, 
               const std::string& f = "", unsigned ln = 0, unsigned col = 0)
        : level(lvl), message(msg), file(f), line(ln), column(col) {}
};

/// Manages diagnostics and error reporting
class DiagnosticManager {
private:
    std::vector<Diagnostic> diagnostics;
    bool hasErrors = false;
    bool hasWarnings = false;
    bool colorOutput = true;
    bool verboseOutput = false;
    
    // LLVM output streams
    llvm::raw_ostream& errorStream;
    llvm::raw_ostream& warningStream;
    llvm::raw_ostream& noteStream;

public:
    DiagnosticManager();
    DiagnosticManager(const DiagnosticManager& other);
    DiagnosticManager& operator=(const DiagnosticManager& other);
    ~DiagnosticManager();

    /// Add a diagnostic message
    void note(const std::string& message, const std::string& file = "", 
              unsigned line = 0, unsigned column = 0);
    void warning(const std::string& message, const std::string& file = "", 
                 unsigned line = 0, unsigned column = 0);
    void error(const std::string& message, const std::string& file = "", 
               unsigned line = 0, unsigned column = 0);
    void fatal(const std::string& message, const std::string& file = "", 
               unsigned line = 0, unsigned column = 0);

    /// Check if there are any errors or warnings
    bool hasError() const { return hasErrors; }
    bool hasWarning() const { return hasWarnings; }
    bool hasDiagnostics() const { return !diagnostics.empty(); }

    /// Print all diagnostics
    void printDiagnostics() const;
    
    /// Print only errors
    void printErrors() const;
    
    /// Print only warnings
    void printWarnings() const;
    
    /// Print only notes
    void printNotes() const;

    /// Clear all diagnostics
    void clear();
    
    /// Get diagnostic count
    size_t getErrorCount() const;
    size_t getWarningCount() const;
    size_t getNoteCount() const;
    size_t getTotalCount() const { return diagnostics.size(); }

    /// Configure output
    void setColorOutput(bool enabled) { colorOutput = enabled; }
    void setVerboseOutput(bool enabled) { verboseOutput = enabled; }

private:
    /// Print a single diagnostic with proper formatting
    void printDiagnostic(const Diagnostic& diag) const;
    
    /// Get color code for diagnostic level
    std::string getColorCode(DiagnosticLevel level) const;
    
    /// Reset color output
    std::string getResetColor() const;
    
    /// Format file location string
    std::string formatLocation(const std::string& file, unsigned line, unsigned column) const;
};

} // namespace swiftc
