module Test exposing (md)


md : String
md =
    """# Title with `inline code`

Some `inline code` followed by some _italic_ text.

## A section with `some inline-code`

Containing some `more inline code`.

## Another _section_
    
With some **bold** text, and a custom element:

<counter/>

A code block:

    code
    more code
        indented code
    unindented code

### A subsection with `some inline-code`

With a second custom element:

<counter/>
"""
