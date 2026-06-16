# Haskell UNIX shell implementation

My take on ["Build Your Own Shell"](https://app.codecrafters.io/courses/shell/overview) from codecrafters.

It uses POSIX instructions to perform basic shell operation

## Implemented features
1. **builtins**: `pwd`, `cd`, `echo`, `type`
2. *executing programs from `$PATH`*
3. *double and single quotes*
4. *backslash* `\`

## TODO
- actual *Abstract Syntax Tree*
- pipes, redirections, etc.
- tab completions
- signals like Ctrl+C 