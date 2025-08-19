#include "DiagnosticManager.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <algorithm>

namespace swiftc {

DiagnosticManager::DiagnosticManager()
    : errorStream(llvm::errs())
    , warningStream(llvm::errs())
    , noteStream(llvm::outs()) {
}

DiagnosticManager::DiagnosticManager(const DiagnosticManager& other)
    : errorStream(llvm::errs())
    , warningStream(llvm::errs())
    , noteStream(llvm::outs())
    , diagnostics(other.diagnostics)
    , hasErrors(other.hasErrors)
    , hasWarnings(other.hasWarnings)
    , colorOutput(other.colorOutput)
    , verboseOutput(other.verboseOutput) {
}

DiagnosticManager& DiagnosticManager::operator=(const DiagnosticManager& other) {
    if (this != &other) {
        diagnostics = other.diagnostics;
        hasErrors = other.hasErrors;
        hasWarnings = other.hasWarnings;
        colorOutput = other.colorOutput;
        verboseOutput = other.verboseOutput;
    }
    return *this;
}

DiagnosticManager::~DiagnosticManager() = default;

void DiagnosticManager::note(const std::string& message, const std::string& file, 
                             unsigned line, unsigned column) {
    diagnostics.emplace_back(DiagnosticLevel::Note, message, file, line, column);
}

void DiagnosticManager::warning(const std::string& message, const std::string& file, 
                               unsigned line, unsigned column) {
    diagnostics.emplace_back(DiagnosticLevel::Warning, message, file, line, column);
    hasWarnings = true;
}

void DiagnosticManager::error(const std::string& message, const std::string& file, 
                             unsigned line, unsigned column) {
    diagnostics.emplace_back(DiagnosticLevel::Error, message, file, line, column);
    hasErrors = true;
}

void DiagnosticManager::fatal(const std::string& message, const std::string& file, 
                             unsigned line, unsigned column) {
    diagnostics.emplace_back(DiagnosticLevel::Fatal, message, file, line, column);
    hasErrors = true;
}

void DiagnosticManager::printDiagnostics() const {
    if (diagnostics.empty()) {
        return;
    }
    
    // Print notes first, then warnings, then errors
    for (const auto& diag : diagnostics) {
        printDiagnostic(diag);
    }
}

void DiagnosticManager::printErrors() const {
    for (const auto& diag : diagnostics) {
        if (diag.level == DiagnosticLevel::Error || diag.level == DiagnosticLevel::Fatal) {
            printDiagnostic(diag);
        }
    }
}

void DiagnosticManager::printWarnings() const {
    for (const auto& diag : diagnostics) {
        if (diag.level == DiagnosticLevel::Warning) {
            printDiagnostic(diag);
        }
    }
}

void DiagnosticManager::printNotes() const {
    for (const auto& diag : diagnostics) {
        if (diag.level == DiagnosticLevel::Note) {
            printDiagnostic(diag);
        }
    }
}

void DiagnosticManager::clear() {
    diagnostics.clear();
    hasErrors = false;
    hasWarnings = false;
}

size_t DiagnosticManager::getErrorCount() const {
    return std::count_if(diagnostics.begin(), diagnostics.end(),
        [](const Diagnostic& d) { 
            return d.level == DiagnosticLevel::Error || d.level == DiagnosticLevel::Fatal; 
        });
}

size_t DiagnosticManager::getWarningCount() const {
    return std::count_if(diagnostics.begin(), diagnostics.end(),
        [](const Diagnostic& d) { return d.level == DiagnosticLevel::Warning; });
}

size_t DiagnosticManager::getNoteCount() const {
    return std::count_if(diagnostics.begin(), diagnostics.end(),
        [](const Diagnostic& d) { return d.level == DiagnosticLevel::Note; });
}

void DiagnosticManager::printDiagnostic(const Diagnostic& diag) const {
    // Choose appropriate output stream
    llvm::raw_ostream& stream = (diag.level == DiagnosticLevel::Note) ? noteStream : errorStream;
    
    // Print diagnostic level with color
    if (colorOutput) {
        stream << getColorCode(diag.level);
    }
    
    // Print level prefix
    switch (diag.level) {
        case DiagnosticLevel::Note:
            stream << "note: ";
            break;
        case DiagnosticLevel::Warning:
            stream << "warning: ";
            break;
        case DiagnosticLevel::Error:
            stream << "error: ";
            break;
        case DiagnosticLevel::Fatal:
            stream << "fatal error: ";
            break;
    }
    
    if (colorOutput) {
        stream << getResetColor();
    }
    
    // Print message
    stream << diag.message;
    
    // Print location if available
    if (!diag.file.empty() || diag.line > 0) {
        stream << "\n";
        if (colorOutput) {
            stream << getColorCode(DiagnosticLevel::Note);
        }
        stream << "  " << formatLocation(diag.file, diag.line, diag.column);
        if (colorOutput) {
            stream << getResetColor();
        }
    }
    
    stream << "\n";
}

std::string DiagnosticManager::getColorCode(DiagnosticLevel level) const {
    if (!colorOutput) return "";
    
    switch (level) {
        case DiagnosticLevel::Note:
            return "\033[36m"; // Cyan
        case DiagnosticLevel::Warning:
            return "\033[33m"; // Yellow
        case DiagnosticLevel::Error:
        case DiagnosticLevel::Fatal:
            return "\033[31m"; // Red
        default:
            return "";
    }
}

std::string DiagnosticManager::getResetColor() const {
    return colorOutput ? "\033[0m" : "";
}

std::string DiagnosticManager::formatLocation(const std::string& file, unsigned line, unsigned column) const {
    std::string result;
    
    if (!file.empty()) {
        result += file;
    }
    
    if (line > 0) {
        if (!result.empty()) result += ":";
        result += std::to_string(line);
        
        if (column > 0) {
            result += ":" + std::to_string(column);
        }
    }
    
    return result;
}

} // namespace swiftc
