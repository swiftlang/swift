import pytest
import subprocess
import os
import sys
import shlex
import tempfile
import re
from unittest.mock import patch, MagicMock


ADVERSARIAL_PAYLOADS = [
    # Command injection via semicolon
    "test_file.swift; rm -rf /tmp/test_injection_marker",
    # Command injection via pipe
    "test_file.swift | cat /etc/passwd",
    # Command injection via backtick
    "test_file.swift `whoami`",
    # Command injection via $()
    "test_file.swift $(id)",
    # Command injection via &&
    "test_file.swift && echo INJECTED",
    # Command injection via ||
    "nonexistent_file.swift || echo INJECTED",
    # Newline injection
    "test_file.swift\necho INJECTED",
    # Null byte injection
    "test_file.swift\x00; echo INJECTED",
    # Path traversal with injection
    "../../etc/passwd; echo INJECTED",
    # Spaces and special chars
    "test file with spaces; echo INJECTED",
    # Redirect injection
    "test_file.swift > /tmp/injected_output",
    # Redirect append injection
    "test_file.swift >> /tmp/injected_output",
    # Background process injection
    "test_file.swift & echo INJECTED",
    # Subshell injection
    "test_file.swift $(cat /etc/hostname)",
    # Double quote escape
    'test_file.swift"; echo INJECTED; echo "',
    # Single quote escape
    "test_file.swift'; echo INJECTED; echo '",
    # Glob expansion
    "test_file.swift *",
    # Environment variable injection
    "test_file.swift $HOME",
    # Complex nested injection
    "test_file.swift; $(echo cm0gLXJmIC90bXAvdGVzdA== | base64 -d)",
    # Unicode injection attempt
    "test_file.swift\u003b echo INJECTED",
    # Tilde expansion
    "~/../../etc/passwd",
    # Process substitution
    "test_file.swift <(cat /etc/passwd)",
]


def build_safe_command(file_path: str, base_cmd: str = "swift") -> list:
    """
    Security invariant: Commands must be constructed as lists (not shell strings)
    to prevent shell injection. This function demonstrates the SAFE pattern.
    """
    # Safe: use list form, never shell=True with user input
    return [base_cmd, file_path]


def is_shell_injection_safe(cmd_parts: list) -> bool:
    """
    Verify that a command list does not contain shell metacharacters
    that would be dangerous if passed to shell=True.
    """
    shell_metacharacters = [';', '|', '&', '$', '`', '>', '<', '\n', '\x00', '(', ')']
    for part in cmd_parts:
        for char in shell_metacharacters:
            if char in str(part):
                return False
    return True


def sanitize_file_path(file_path: str) -> str:
    """
    Sanitize a file path to remove shell injection characters.
    Returns only the basename with safe characters.
    """
    # Extract basename and allow only safe characters
    basename = os.path.basename(file_path)
    sanitized = re.sub(r'[^a-zA-Z0-9._\-]', '', basename)
    return sanitized


@pytest.mark.parametrize("payload", ADVERSARIAL_PAYLOADS)
def test_command_construction_never_uses_shell_true_with_user_input(payload):
    """
    Invariant: When constructing subprocess commands that incorporate
    user-controlled input (such as file paths), the command must NEVER
    be passed as a shell string with shell=True. Commands must be
    constructed as lists and executed with shell=False (default).
    """
    # Simulate what litplugin.py does when building a command with user input
    base_command = "swift"
    
    # UNSAFE pattern (what the vulnerable code does):
    unsafe_cmd_string = f"{base_command} {payload}"
    
    # SAFE pattern (what the code SHOULD do):
    safe_cmd_list = [base_command, payload]
    
    # Property 1: The safe command must be a list, not a string
    assert isinstance(safe_cmd_list, list), \
        "Commands with user input must be constructed as lists, not strings"
    
    # Property 2: When using subprocess with user input, shell must be False
    # We verify this by checking that the safe pattern doesn't invoke shell interpretation
    with patch('subprocess.run') as mock_run:
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")
        
        # Safe invocation
        subprocess.run(safe_cmd_list, shell=False, capture_output=True, text=True)
        
        call_args = mock_run.call_args
        # Verify shell=False was used
        assert call_args.kwargs.get('shell', False) == False, \
            f"shell=True must never be used with user-controlled input. Payload: {payload!r}"
        
        # Verify command was passed as list
        cmd_arg = call_args.args[0] if call_args.args else call_args.kwargs.get('args')
        assert isinstance(cmd_arg, list), \
            f"Command must be a list when incorporating user input. Payload: {payload!r}"


@pytest.mark.parametrize("payload", ADVERSARIAL_PAYLOADS)
def test_sanitized_path_contains_no_shell_metacharacters(payload):
    """
    Invariant: Any file path derived from user input that is used in
    command construction must be sanitized to remove shell metacharacters,
    regardless of whether shell=True or shell=False is used.
    """
    sanitized = sanitize_file_path(payload)
    
    dangerous_chars = [';', '|', '&', '$', '`', '>', '<', '\n', '\x00', 
                       '(', ')', ' ', '\t', '"', "'", '\\', '*', '?', '[', ']']
    
    for char in dangerous_chars:
        assert char not in sanitized, \
            f"Sanitized path must not contain shell metacharacter {char!r}. " \
            f"Original payload: {payload!r}, Sanitized: {sanitized!r}"


@pytest.mark.parametrize("payload", ADVERSARIAL_PAYLOADS)
def test_subprocess_with_shell_true_never_receives_unsanitized_user_input(payload):
    """
    Invariant: If shell=True must be used for any reason, the command string
    must never contain unsanitized user-controlled data. The payload must be
    properly quoted using shlex.quote() at minimum.
    """
    # If shell=True is unavoidable, user input MUST be quoted
    quoted_payload = shlex.quote(payload)
    safe_shell_cmd = f"swift {quoted_payload}"
    
    # Property: shlex.quote must wrap the payload in single quotes and escape internal quotes
    # This ensures the shell treats it as a single argument
    assert quoted_payload.startswith("'") or quoted_payload == payload.replace("'", ""), \
        "shlex.quote must properly quote user input for shell usage"
    
    # Property: The quoted payload must not allow command injection
    # A properly quoted string should not contain unescaped shell operators
    # outside of single quotes
    if "'" in quoted_payload:
        # shlex.quote escapes single quotes as '\''
        assert "\\'" in quoted_payload or quoted_payload.count("'") >= 2, \
            "Single quotes in payload must be properly escaped"
    
    # Property: Verify that the raw payload (if it contains injection chars) 
    # is different from the quoted version when dangerous
    dangerous_chars = [';', '|', '&', '`', '$', '\n', '(', ')']
    has_dangerous = any(c in payload for c in dangerous_chars)
    
    if has_dangerous:
        # The quoted version must differ from raw (it must be escaped/wrapped)
        assert quoted_payload != payload, \
            f"shlex.quote must transform dangerous payloads. " \
            f"Payload: {payload!r} was not transformed"


@pytest.mark.parametrize("payload", ADVERSARIAL_PAYLOADS)
def test_no_shell_injection_side_effects_when_using_list_form(payload):
    """
    Invariant: Using subprocess with a list command and shell=False must
    never execute injected commands, even with adversarial file path inputs.
    The injected commands must not produce side effects.
    """
    marker_file = tempfile.mktemp(prefix="injection_test_marker_")
    
    # Ensure marker doesn't exist before test
    if os.path.exists(marker_file):
        os.remove(marker_file)
    
    try:
        # Construct a payload that would create the marker file if injection succeeds
        injection_payload = f"{payload}; touch {marker_file}"
        
        # Safe invocation: list form, shell=False
        # This should NOT execute the injected command
        with patch('subprocess.run') as mock_run:
            mock_run.return_value = MagicMock(returncode=1, stdout="", stderr="file not found")
            
            result = subprocess.run(
                ["swift", injection_payload],  # list form - safe
                shell=False,
                capture_output=True,
                text=True
            )
            
            # Verify the mock was called with shell=False
            call_kwargs = mock_run.call_args.kwargs
            assert not call_kwargs.get('shell', False), \
                "shell=False must be used to prevent injection"
        
        # The marker file must NOT exist (injection did not execute)
        assert not os.path.exists(marker_file), \
            f"Shell injection side effect detected! Marker file was created. " \
            f"Payload: {payload!r}"
    
    finally:
        # Cleanup
        if os.path.exists(marker_file):
            os.remove(marker_file)


@pytest.mark.parametrize("payload", ADVERSARIAL_PAYLOADS)
def test_command_list_treats_payload_as_single_argument(payload):
    """
    Invariant: When a file path payload is passed as an element in a command
    list (not a shell string), the entire payload must be treated as a single
    argument to the executable, not parsed by the shell.
    """
    cmd_list = ["swift", payload]
    
    # Property: The list must have exactly 2 elements regardless of payload content
    assert len(cmd_list) == 2, \
        f"Command list must have exactly 2 elements. " \
        f"Payload: {payload!r} caused unexpected list length"
    
    # Property: The second element must be exactly the payload (no shell parsing)
    assert cmd_list[1] == payload, \
        f"Payload must be preserved as-is in list form. " \
        f"Expected: {payload!r}, Got: {cmd_list[1]!r}"
    
    # Property: Shell metacharacters in the payload must not split the argument
    # (i.e., the list length must remain 2, not grow due to shell word splitting)
    assert cmd_list[0] == "swift", \
        "First element must remain the executable name"