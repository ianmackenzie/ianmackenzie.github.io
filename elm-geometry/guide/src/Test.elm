module Test exposing (emu)


emu : String
emu =
    """
| Section
    First section

Some {Elm|inline code} followed by some /italic/ text

| Subsection
    A subsection

Containing some {Elm|more inline code}

| Section
    Second section
    
With some *bold* text

A code block:

| Elm
    code
    more code
        indented code
    unindented code

| Subsection
    Another subsection
"""
        |> String.trimLeft
